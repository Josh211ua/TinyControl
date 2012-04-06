module TinyControl.Server
  ( Data(..),
    serveData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as P
import qualified TinyControl.Time as T

import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

import System.Timeout(timeout)
import Control.Concurrent(myThreadId, forkIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket (
  Socket
  , SockAddr
  , getAddrInfo
  , socket
  , SocketType(Datagram)
  , withSocketsDo
  , sClose
  , sendTo
  , AddrInfo(..)
  , AddrInfoFlag(AI_PASSIVE)
  , defaultHints
  , recvFrom
  , bindSocket
  , getNameInfo)
import Network.BSD (HostName, defaultProtocol)
import Debug.Trace(trace)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (genericDrop, maximumBy)
data ServerState = ServerState { rto :: NominalDiffTime -- time between nfdbkTimer expirations
                               , tld :: UTCTime         -- time last doubled
                               , r :: Maybe NominalDiffTime -- estimate of RTT
                               , x_recvset :: [(UTCTime, Int)] -- set of Xrecvs
                               , x :: Int -- send rate, bytes per sec
                               , sendMoreTime :: UTCTime
                               , howManyMore :: Int
                               , noFeedBackTime :: UTCTime
                               }
                               deriving (Show)
type TimeStamp = UTCTime

type ServerHandle = Handle ServerState

type ServerStateMonad = RWST (Socket,Friend) [String] ServerState IO
type HandlerFunction = Data -> IO Data


serveData :: String -> HandlerFunction -> IO ()
serveData port f = do
  h <- open port
  serve h
  where
    serve :: Socket -> IO ()
    serve h = do
      (handle, friend, msg) <- srecv h
      threadId <- forkIO (serverThread handle friend msg)
      serve h
    serverThread :: Socket -> Friend -> Data -> IO ()
    serverThread sock friend msg = do
      resp <- f msg
      now <- T.now
      noFeedBackTimer <- T.nextTimeoutSec 2
      let (packetPerInterval, sendMoreT) = howMuchAndWhen P.s now
      let theState = ServerState { rto = T.sToDiffTime 2
                  , tld = now
                  , r = Nothing
                  , x_recvset = []
                  , x = P.s
                  , sendMoreTime = sendMoreT
                  , howManyMore = packetPerInterval
                  , noFeedBackTime = noFeedBackTimer
                  }
      (a,s,w) <- runRWST (serverThreadHelper resp) (sock,friend) theState
      sClose sock
    howMuchAndWhen :: Int -> UTCTime -> (Int, UTCTime)
    howMuchAndWhen bytesPerSecond now =
      let fractionalP =bytesPerSecond / P.s in
      let amount = ceiling fractionalP in
      let seconds = ceiling $ intToFloat (amount * P.s) / intToFloat bytesPerSecond in
      (amount, T.nextTimeoutSecPure seconds now)
    serverThreadHelper :: Data -> ServerStateMonad ()
    serverThreadHelper m | (ByteString.length m) == 0 = (trace "done") return ()
    serverThreadHelper msg = do
      (sock, friend) <- ask
      let (mmsg, rest) = ByteString.splitAt P.s msg
      now <- lift T.now
      let rmsg = P.DataPacket {
          P.seqNum = 0,
          P.timeStamp = now,
          P.rtt = 2,
          P.payload = mmsg
          }
      lift $ send sock friend (show rmsg)
      (serverThreadHelper rest)

-- Helper Methods
expireNoFeedbackTimer :: ServerStateMonad ()
expireNoFeedbackTimer = undefined

handlePacket :: P.FeedbackPacket ->  ServerStateMonad ()
handlePacket pack = do
    ss <- get
    t_now <- lift $ T.now
    let r_sample = calculateRSample t_now (P.t_recvdata pack) (P.t_delay pack)
    let r' = updateR (r ss) r_sample
    let rto' = updateRto r' (x ss)
    let x_recvset' = updateXRecvset (x_recvset ss) (r') (t_now)
    let (x', tld') = updateRate (x_recvset') (x ss) 
            (r') (P.p pack) (t_now) (tld ss)
    put ss { r = Just r'
           , rto = rto'
           , x_recvset = x_recvset'
           , x = x'
           , tld = tld'
           }
    error "Must reset no feedback timer to rto seconds"

intToFloat :: Int -> Float
intToFloat x = fromInteger $ toInteger x

calculateRSample :: UTCTime -> UTCTime -> Int -> NominalDiffTime
calculateRSample t_now t_recvdata t_delay = do
    T.sToDiffTime $ (T.diffTimeToS $ t_now `diffUTCTime` t_recvdata) - t_delay

q :: Float
q = 0.9

updateR :: Maybe NominalDiffTime -> NominalDiffTime -> NominalDiffTime
updateR r_old r_sample = case (r_old) of
    Nothing -> r_sample
    Just (r) -> T.sToDiffTime $ floor $
        q * (fromInteger $ toInteger $ T.diffTimeToS r) + 
        (1 - q) * (fromInteger $ toInteger $ T.diffTimeToS r_sample)

updateRto :: NominalDiffTime -> Int -> NominalDiffTime
updateRto r x = T.sToDiffTime $ max 
    (4 * (T.diffTimeToS r)) 
    (floor $ (intToFloat $ 2 * P.s) / (intToFloat $ x))

t_mbi :: Float
t_mbi = 64

w_init :: Float
w_init = intToFloat $ min (4 * P.s) (max (2 * P.s) (4380))

initialRate :: NominalDiffTime -> Float
initialRate r = (w_init) / (intToFloat $ T.diffTimeToS r)

updateXRecvset :: [(UTCTime, Int)] -> NominalDiffTime -> UTCTime -> [(UTCTime, Int)]
updateXRecvset x_recvset r t_now = 
  let earliestTime = (negate r) `addUTCTime` t_now
  in filter (timesAfter earliestTime) x_recvset
     where timesAfter = undefined

updateRate :: [(UTCTime, Int)] -> Int -> NominalDiffTime -> 
    Float -> UTCTime -> UTCTime -> (Int, UTCTime)
updateRate x_recvset x r p t_now tld = 
    if p > 0
      then ((xMaxMin (xBps r p) (floor $ ((intToFloat P.s) / t_mbi))), tld)
      else if (t_now `diffUTCTime` tld) >= r
        then ((xMaxMin (2 * x) (floor $ initialRate r)), t_now)
        else (x, tld)
    where recv_limit = snd $ maximumBy (\(x1,y1) (x2,y2) -> compare y1 y2) x_recvset
          xMaxMin :: Int -> Int -> Int
          xMaxMin a b = max (min (a) (recv_limit)) (b)
    
xBps :: NominalDiffTime -> Float -> Int
xBps r p = let r' = (intToFloat $ T.diffTimeToS r)
  in floor $ (intToFloat P.s) / 
        (r' * (sqrt (2 * p / 3)) + 12 * (sqrt (3 * p / 8)) * p * (1 + 32 * p^2))

-- Network Operations
open :: String -> IO (Socket)
open port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo
                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                      Nothing (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       theSock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       bindSocket theSock (addrAddress serveraddr)

       -- Send back the handle
       return theSock


srecv :: Socket -> IO (Socket, Friend, Data)
srecv s =
  withSocketsDo $
  do
    result <- receiveHelper s
    let (friend, val) = result

    (host, service) <- getNameInfo [] True True friend
    addrinfos <- getAddrInfo (Just defaultHints) host service
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

    return $ (sock, friend, val)

  where
    receiveHelper :: Socket -> IO (Friend, Data) -- t m a
    receiveHelper sock = do
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
      (msg, num, addr) <- (recvFrom sock 1024)
      --tell (["Srecv'd: " ++ msg])
      let d = (pack msg)
      return (addr, d)


recv :: ServerStateMonad (P.FeedbackPacket)
recv = do
    (sock, friend) <- ask
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
    mresult <- lift $ timeout 5 (recvFrom sock 1024)
    case mresult of
      Nothing -> error "timer expired"
      Just (msg, _, _) -> do
        tell (["Recv'd: " ++ msg])
        return $ read msg

send :: Socket -> Friend -> String -> IO ()
send a friend msg = C.sendstr a friend msg

