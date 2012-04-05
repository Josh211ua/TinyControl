module TinyControl.Client
  ( Data(..)
  , wantData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as P
import TinyControl.Packet (DataPacket)
import TinyControl.Time (diffTimeToS, sToDiffTime, getTimeout, nextTimeout, toUs)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

import Data.List (sortBy)
import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket (
  Socket
  , SockAddr
  , getAddrInfo
  , socket
  , SocketType(Datagram)
  , withSocketsDo
  , sClose
  , AddrInfo(..)
  , AddrInfoFlag(AI_PASSIVE)
  , defaultHints
  , recvFrom)
import Network.BSD (HostName, defaultProtocol)
import System.Timeout(timeout)
import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime, addUTCTime)
import Data.Set (Set)
import qualified Data.Set as Set

type SeqNum = Int
type Interval = Int
type TimeStamp = UTCTime
type PacketStamp = (SeqNum, TimeStamp)
data PreLossEvent = PreLossEvent { before :: PacketStamp
                                 , after :: PacketStamp
                                 , mdu :: Int
                                 } deriving (Show, Read)

data ClientState = ClientState { intervals :: [Interval]
                               , packetHistory :: [ PreLossEvent ]
                               , lastLossEvent :: Maybe PacketStamp
                               , lastPacketStamp :: PacketStamp
                               , lastPacket :: DataPacket
                               , nextTimeoutTime :: UTCTime
                               , p :: Float
                               , x_recv :: Int
                               }
                               deriving (Show, Read)

type ClientStateMonad = RWST (Socket, Friend) [(SeqNum,ByteString)] ClientState IO

wantData :: String -> String -> Data -> IO Data
wantData host port msg = do
    (sock, friend) <- open host port
    send sock friend (unpack msg)
    firstPacket sock

firstPacket :: Socket -> IO Data
firstPacket sock = do
    (msg, _, friend) <- recvFrom sock P.dataPacketSize
    t_delay_start <- getCurrentTime
    -- read data packet
    let packet = read msg
    if P.isLastDataPacket packet
       then return $ P.payload packet
       else do
         -- initialize state from pack
         let ss = initialState packet t_delay_start
         -- send Feedback Packet
         makeAndSendFeedbackPacket sock friend ss
         -- determine timeout interval
         futureTimeout <- nextTimeout (P.rtt packet)
         let ss' = ss {nextTimeoutTime = futureTimeout}
         -- begin running through states
         let nextPacket = receiveNextPacket sock ss'
         (_, _, w) <- runRWST (m1 nextPacket) (sock, friend) ss'
         return $ foldr (\(_, bs) acc -> acc `append` bs) (P.payload packet) (sortBy (\(s1,_) (s2,_) -> compare s1 s2) w)

initialState :: P.DataPacket -> UTCTime -> ClientState
initialState dp t_delay_start = ClientState {
     intervals = []
   , packetHistory = []
   , lastLossEvent = Nothing
   , lastPacket = dp
   , lastPacketStamp = (P.seqNum dp, t_delay_start)
   , nextTimeoutTime = read "0000-00-00 00:00:00"
   , p = 0
   , x_recv = 0
   }

receiveNextPacket :: Socket -> ClientState -> IO (Maybe (String, Int, SockAddr))
receiveNextPacket sock ss = do
    timeoutInterval <- getTimeout (nextTimeoutTime ss)
    timeout (toUs timeoutInterval) (recvFrom sock P.dataPacketSize)

m1 :: IO (Maybe (String, Int, SockAddr)) -> ClientStateMonad ()
m1 result = do
    lift $ print "entering m1"
    (sock, f) <- ask
    ss <- get
    maybeD <- lift result
    case (maybeD) of
         Just (d, _, _) -> do
             let p = read d
             if P.isLastDataPacket p
                then return ()
                else do
                    gotDataPacket p False
                    let nextPacket = receiveNextPacket sock ss
                    m2 nextPacket
         Nothing -> do
             expireFeedbackTimer
             let nextPacket = receiveNextPacket sock ss
             m3 nextPacket

m2 :: IO (Maybe (String, Int, SockAddr)) -> ClientStateMonad ()
m2 result = do
    lift $ print "entering m2"
    (sock, f) <- ask
    ss <- get
    maybeD <- lift result
    case (maybeD) of
         Just (d, _, _) -> do
             let p = read d
             if P.isLastDataPacket p
                then return ()
                else do
                    gotDataPacket p False
                    let nextPacket = receiveNextPacket sock ss
                    m2 nextPacket
         Nothing -> do
             expireFeedbackTimer
             lift $ makeAndSendFeedbackPacket sock f ss
             let nextPacket = receiveNextPacket sock ss
             m1 nextPacket

m3 :: IO (Maybe (String, Int, SockAddr)) -> ClientStateMonad ()
m3 result = do
    lift $ print "entering m3"
    (sock, f) <- ask
    ss <- get
    maybeD <- lift result
    case (maybeD) of
         Just (d, _, _) -> do
             let p = read d
             if P.isLastDataPacket p
                then return ()
                else do
                    gotDataPacket p True
                    expireFeedbackTimer
                    let nextPacket = receiveNextPacket sock ss
                    m1 nextPacket
         Nothing -> do
             resetFeedbackTimer
             let nextPacket = receiveNextPacket sock ss
             m3 nextPacket

makeAndSendFeedbackPacket :: Socket -> Friend -> ClientState -> IO ()
makeAndSendFeedbackPacket sock friend ss = do
    feedbackPacket <- makeFeedbackPacket ss
    sendFeedbackPacket sock friend feedbackPacket

makeFeedbackPacket :: ClientState -> IO P.FeedbackPacket
makeFeedbackPacket s = do
    now <- getCurrentTime
    return P.FeedbackPacket {
        P.t_recvdata = (P.timeStamp (lastPacket s))
      , P.t_delay = diffTimeToS $ now `diffUTCTime` snd (lastPacketStamp s)
      , P.x_recv = x_recv s
      , P.p = p s }

sendFeedbackPacket :: Socket -> Friend -> P.FeedbackPacket -> IO ()
sendFeedbackPacket sock friend packet = do
    -- TODO cut out middle man, this should always be exactly 16 bytes once
    --  we are sending correctly
    send sock friend (show packet)

gotDataPacket :: DataPacket -> Bool -> ClientStateMonad ()
gotDataPacket pack inM3 = do
    ss <- get
    timeStamp <- lift $ getCurrentTime
    tell [((P.seqNum pack),(P.payload pack))]
    addToPacketHistory pack timeStamp
    loss <- checkForLoss
    if loss || inM3
       then do
           let p' = calculateP $ intervals ss
           let p_prev = p ss
           put $ ss { p = p' }
           if (p' > p_prev)
              then expireFeedbackTimer
              else return ()
       else return ()

addToPacketHistory :: DataPacket -> TimeStamp -> ClientStateMonad ()
addToPacketHistory dp newT = do
    ss <- get
    let newSeq = P.seqNum dp
    let (oldSeq, oldT) = lastPacketStamp ss
    let oldPacketHistory = packetHistory ss
    case (compare newSeq (oldSeq + 1)) of
         EQ -> put ss { lastPacketStamp = (newSeq, newT), lastPacket = dp }
         GT -> do
            let newLossEvent = PreLossEvent {
                 before = (oldSeq, oldT)
               , after = (newSeq, newT)
               , mdu = 0
               }
            let newHistory = newLossEvent:oldPacketHistory
            put ss { packetHistory = newHistory
                   , lastPacketStamp = (newSeq, newT)
                   , lastPacket = dp
                   }
         LT -> put ss {packetHistory = findAndFillGap oldPacketHistory (newSeq, newT)}
              where
                findAndFillGap :: [PreLossEvent] -> PacketStamp -> [PreLossEvent]
                findAndFillGap history new = let (start,end) = break (inGap new) history in
                  case end of
                    [] -> history
                    (match:rest) -> start ++ (fillGap new match) ++ rest
                  where
                    inGap :: PacketStamp -> PreLossEvent -> Bool
                    inGap (s,t) (PreLossEvent {before=(s1,_),after=(s2,_)}) = and [compare s1 s == LT, compare s2 s == GT]
                    fillGap :: PacketStamp -> PreLossEvent -> [PreLossEvent]
                    fillGap c@(sc,_) ab = let a@(sa,_) = before ab in let b@(sb,_) = after ab in
                      case (sa,sc,sb) of
                        --assuming c/= b && c /= a
                        _| and [sa + 1 == sc, sc + 1 == sb] -> [] --closed gap
                        _| sa + 1 == sc -> [ab {before=c}] --is at start of gap
                        _| sb - 1 == sc -> [ab {after=c}] --is at end of gap
                        _ -> [ab {before=c},ab {after=c}] --split case
    return ()

mduC :: Int
mduC = 3

incrementMDUs :: SeqNum -> [PreLossEvent] -> [PreLossEvent]
incrementMDUs s oldPacketHistory = do
    ple <- oldPacketHistory
    let (afterSeq, _) = after ple
    if (afterSeq < s)
       then return $ ple {mdu = (mdu ple) + 1}
       else return ple

checkForLoss :: ClientStateMonad (Bool)
-- Note must also deal with the loss here
checkForLoss = do
    ss <- get
    let (lastPacketSeq, _) = lastPacketStamp ss
    let incrementHistory = incrementMDUs (lastPacketSeq) (packetHistory ss)
    let (newHistory, lossEvent) = span (\x -> mdu x <= mduC) incrementHistory
    put $ ss {packetHistory = newHistory}
    if null lossEvent
       then return False
       else do
           dealWithLossEvents lossEvent
           return True

dealWithLossEvents :: [PreLossEvent] -> ClientStateMonad ()
dealWithLossEvents lossEvents = do
    let sortedPackets = reverse lossEvents
    dealWithRest lossEvents
    where -- TODO deal with loss ranges of length > 1
        dealWithRest [] = return ()
        dealWithRest (PreLossEvent{ before = b
                                  , after = a
                                  ,  mdu = m}:xs) = do
          ss <- get
          let sLoss = (fst b) + 1
          let tLoss' = tLoss b a sLoss
          let lastPacket' = lastPacket ss
          case lastLossEvent ss of
            Just (_, tOld) -> if (sToDiffTime (P.rtt lastPacket') `addUTCTime` tOld) >= tLoss'
                                 then do
                                    newLossInterval b a
                                    dealWithRest xs
                                 else dealWithRest xs
            Nothing -> error "I have no idea what to put here"

tLoss :: PacketStamp -> PacketStamp -> SeqNum -> TimeStamp
-- Going to assume sLoss is one after sBegin
tLoss = undefined

newLossInterval = undefined

expireFeedbackTimer :: ClientStateMonad ()
expireFeedbackTimer = do
    (sock,f) <- ask
    ss <- get
    let p' = calculateP (intervals ss)
    let x_recv' = calculateMeasuredReceiveRate
    put $ ss {p = p', x_recv = x_recv'}
    lift $ makeAndSendFeedbackPacket sock f ss
    resetFeedbackTimer

resetFeedbackTimer :: ClientStateMonad  ()
resetFeedbackTimer = do
  ss <- get
  let lastPack = lastPacket ss
  futureTimeout <- lift $ nextTimeout $ P.rtt lastPack
  put $ ss { nextTimeoutTime = futureTimeout }

-- Calculate P:
calculateP :: [Interval] -> Float
calculateP intervals = 1 / (iMean intervals)

n :: Int
n = 8

wI :: Int -> Float
wI i = let nRat = fromInteger $ toInteger n in
   let iRat = fromInteger $ toInteger i in
   if iRat < nRat / 2
      then 1
      else 2 * (nRat - iRat) / (nRat + 2)

wS :: [Float]
wS = [wI i | i <- [1..n]]

iMean :: [Interval] -> Float
iMean intervals = (fromInteger $ toInteger $ iTot intervals) / wTot

iTot :: [Interval] -> Int
iTot intervals = sum intervals

wTot :: Float
wTot = sum wS

-- Calculate x_recv:
calculateMeasuredReceiveRate = undefined

-- UDP Operations
open :: String -> String -> IO (Socket, Friend)
open hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       theSock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       --let friend = serveraddr

       -- Send back the handle
       return (theSock, addrAddress serveraddr)

send :: Socket -> Friend -> String -> IO ()
send sock friend msg = C.sendstr sock friend msg

close :: Socket -> IO ()
close s = sClose s
