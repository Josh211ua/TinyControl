module TinyControl.Client
  ( Data(..)
  , wantData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as P
import TinyControl.Packet (DataPacket)
import TinyControl.Time (diffTimeToS, getTimeout, nextTimeout, toUs)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

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
import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Set (Set)
import qualified Data.Set as Set

data ClientState = ClientState { lastDataPacket :: P.DataPacket
                               , lastDataPacketTime :: UTCTime
                               , nextTimeoutTime :: UTCTime
                               , p :: Float
                               , x_recv :: Int
                               }
                               deriving (Show, Read)

type SeqNum = Int
type Packet = (SeqNum, UTCTime)
type PreLossEvent =(Packet, Packet) --- P_before, P_after
type PacketHistory = (SeqNum, [PreLossEvent])

type ClientStateMonad = RWST (Socket, Friend) ByteString ClientState IO

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
         return $ (P.payload packet) `append` w

initialState :: P.DataPacket -> UTCTime -> ClientState
initialState dp t_delay_start = ClientState {
     lastDataPacket = dp
   , lastDataPacketTime = t_delay_start
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
                    gotDataPacket p
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
                    gotDataPacket p
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
                    gotDataPacket p
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
        P.t_recvdata = (P.timeStamp (lastDataPacket s))
      , P.t_delay = diffTimeToS $ now `diffUTCTime` (lastDataPacketTime s)
      , P.x_recv = x_recv s
      , P.p = p s }

sendFeedbackPacket :: Socket -> Friend -> P.FeedbackPacket -> IO ()
sendFeedbackPacket sock friend packet = do
    -- TODO cut out middle man, this should always be exactly 16 bytes once
    --  we are sending correctly
    send sock friend (show packet)

gotDataPacket :: DataPacket -> ClientStateMonad ()
gotDataPacket p = do
    tell (P.payload p)
    --addToPacketHistory
    --if done
    return ()
    --else

expireFeedbackTimer :: ClientStateMonad ()
expireFeedbackTimer = do
    (sock,f) <- ask
    ss <- get
    let p' = calculateAverageLossEventRate
    let x_recv' = calculateMeasuredReceiveRate
    put $ ss {p = p', x_recv = x_recv'}
    lift $ makeAndSendFeedbackPacket sock f ss
    resetFeedbackTimer

resetFeedbackTimer :: ClientStateMonad  ()
resetFeedbackTimer = do
  ss <- get
  let lastPack = lastDataPacket ss
  futureTimeout <- lift $ nextTimeout $ P.rtt lastPack
  put $ ss { nextTimeoutTime = futureTimeout }

calculateAverageLossEventRate = undefined
calculateMeasuredReceiveRate = undefined

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
