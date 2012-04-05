module TinyControl.Server
  ( Data(..),
    serveData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as P
import qualified TinyControl.Time as T

--import System.Time (TimeDiff(..), CalendarTime, getClockTime, toCalendarTime)
import Data.Time.Clock (NominalDiffTime)
import Data.Time (UTCTime)


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

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (genericDrop)
data ServerState = ServerState { rto :: NominalDiffTime
                               , tld :: UTCTime
                               , r :: Maybe NominalDiffTime
                               , x_recvset :: Set (UTCTime, Int)
                               , x :: Int
                               }
                               deriving (Show)

type ServerHandle = Handle ServerState

type ServerStateMonad r = RWST r [String] ServerState IO
type HandlerFunction = Data -> IO Data

serveData :: String -> HandlerFunction -> IO ()
serveData port f = do
  h <- open port
  serve h
  where
    serve :: ServerHandle -> IO ()
    serve h = do
      (handle, friend, msg) <- srecv h
      threadId <- forkIO (serverThread handle friend msg)
      serve h
    serverThread :: ServerHandle -> Friend -> Data -> IO ()
    serverThread handle friend msg = do
      resp <- f msg
      serverThreadHelper handle friend resp
      close handle
    serverThreadHelper :: ServerHandle -> Friend -> Data -> IO ()
    serverThreadHelper _ _ m | m == ByteString.empty = return ()
    serverThreadHelper handle friend msg = do
      let (mmsg, rest) = ByteString.splitAt P.s msg
      now <- T.now
      let rmsg = P.DataPacket {
          P.seqNum = 0,
          P.timeStamp = now,
          P.rtt = 0,
          P.payload = mmsg
          }
      handle <- send handle friend (show rmsg)
      (serverThreadHelper handle friend rest)


open :: String -> IO (ServerHandle)
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
       --let friend = serveraddr
       bindSocket theSock (addrAddress serveraddr)
       -- Initailize State
       now <- T.now
       let theState = ServerState {
            rto = T.sToDiffTime 2,
            tld = now,
            r = Nothing,
            x_recvset = Set.empty,
            x = P.s
            }

       -- Send back the handle
       return $ Handle { sock=theSock, state=theState }


srecv :: ServerHandle -> IO (ServerHandle, Friend, Data)
srecv h@(Handle {sock = s , state = ss}) =
  withSocketsDo $
  do
    result <- runRWST receiveHelper s ss
    let ((friend, val), state,_) = result
    --addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing Nothing
    (host, service) <- getNameInfo [] True True friend
    addrinfos <- getAddrInfo (Just defaultHints) host service
    let serveraddr = head addrinfos
     -- Establish a socket for communication
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    return $ (Handle {sock = sock, state = state}, friend, val)
    -- Change addr to a new port

  where
    receiveHelper :: ServerStateMonad (Socket) (Friend, Data) -- t m a
    receiveHelper = do
      sock <- ask
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
      (msg, num, addr) <- lift (recvFrom sock 1024)
      tell (["Recv'd: " ++ msg])
      let d = (pack msg)
      return (addr, d)


recv :: ServerHandle -> IO (ServerHandle, Friend, Data)
recv h@(Handle {sock = a, state = ss}) =   withSocketsDo $
  do
    ((friend, val), state,_) <- runRWST receiveHelper a ss
    return $ (Handle {sock = a, state = state}, friend, val)

  where
    receiveHelper :: ServerStateMonad (Socket) (Friend, Data) -- t m a
    receiveHelper = do
      sock <- ask
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
      mresult <- lift $ timeout 5 (recvFrom sock 1024)
      case mresult of
        Nothing -> error "timer expired"
        Just (msg, num, addr) -> do
          tell (["Recv'd: " ++ msg])
          let d = (pack msg)
          return (addr, d)


send :: ServerHandle -> Friend -> String -> IO (ServerHandle)
send h@(Handle {sock = a, state = ss}) friend msg = do
  C.sendstr a friend msg
  return h

close :: ServerHandle -> IO ()
close (Handle {sock = s , state = _}) = sClose (s)
