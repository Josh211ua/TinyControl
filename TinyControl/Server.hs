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
                               , remainingMsg :: ByteString
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
                  , x_recvset = Set.empty
                  , x = P.s
                  , sendMoreTime = sendMoreT
                  , howManyMore = packetPerInterval
                  , noFeedBackTime = noFeedBackTimer
                  , remainingMsg = resp
                  }
      (a,s,w) <- runRWST (serverThreadHelper) (sock,friend) theState
      sClose sock
    howMuchAndWhen :: Int -> UTCTime -> (Int, UTCTime)
    howMuchAndWhen bytesPerSecond now =
      let fractionalP =bytesPerSecond / P.s in
      let amount = ceiling fractionalP in
      let seconds = ceiling $ intToFloat (amount * P.s) / intToFloat bytesPerSecond in
      (amount, T.nextTimeoutSecPure seconds now)
    serverThreadHelper :: ServerStateMonad ()
    serverThreadHelper = do
      state <- get
      let numLeft = howManyMore state
      let msg = remainingMsg state
      case (numLeft, msg) of
        (0, _) -> recv
        (_, m) | (ByteString.length m) == 0 -> (trace "done") return ()
        (pNum, m) -> do
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
    state <- get
    let sendTime = sendMoreTime state
    let feedBackTime = noFeedBackTime state
    let stopTime = soonerTime sendTime feedBackTime
    timeoutInterval <- lift $ T.getTimeout stopTime
    (sock, friend) <- ask
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
    mresult <- lift $ timeout timeoutInterval (recvFrom sock 1024)
    case mresult of
      Nothing -> if sendTime `soonerThan` feedBackTime
        then undefined -- reset howMuchMore/sendTimer; goto send
        else undefined -- reset NOFEEDBACKTIMER; goto recv
      Just (msg, _, _) -> do
        tell (["Recv'd: " ++ msg])
        let feedbackPacket = (read msg)::P.FeedbackPacket
        -- TODO: calculate stats based on feedback
        recv
    where
      soonerTime x y = if x `soonerThan` y then x else y
      soonerThan x y = undefined

send :: Socket -> Friend -> String -> IO ()
send a friend msg = C.sendstr a friend msg

