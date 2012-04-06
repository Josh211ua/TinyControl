module TinyControl.Server
  ( Data(..),
    serveData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as P
import qualified TinyControl.Time as T

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)


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
import Data.List (genericDrop)
data ServerState = ServerState { rto :: NominalDiffTime -- time between nfdbkTimer expirations
                               , tld :: UTCTime         -- time last doubled
                               , r :: Maybe NominalDiffTime -- estimate of RTT
                               , x_recvset :: Set (UTCTime, Int) -- set of Xrecvs
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
      (packetPerInterval, sendMoreT) <- howMuchAndWhen P.s
      let theState = ServerState { rto = T.sToDiffTime 2
                  , tld = now
                  , r = Nothing
                  , x_recvset = Set.empty
                  , x = P.s
                  , sendMoreTime = sendMoreT
                  , howManyMore = packetPerInterval
                  , noFeedBackTime = noFeedBackTimer
                  }
      (a,s,w) <- runRWST (serverThreadHelper resp) (sock,friend) theState
      sClose sock
    howMuchAndWhen :: Int -> IO (Int, UTCTime)
    howMuchAndWhen bytesPerSecond =
      let fractionalP =bytesPerSecond / P.s in
      let amount = ceiling fractionalP in
      let seconds = (amount * P.s) / bytesPerSecond in
      do
        time <- T.nextTimeoutSec seconds
        return (amount, time)
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
    r_sample <- lift $ calculateRSample (P.t_recvdata pack) (P.t_delay pack)
    error "Not implemented"

calculateRSample :: UTCTime -> Int -> IO NominalDiffTime
calculateRSample t_recvdata t_delay = do
    t_now <- T.now
    return (T.sToDiffTime $ (T.diffTimeToS $ t_now `diffUTCTime` t_recvdata) - t_delay)




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

