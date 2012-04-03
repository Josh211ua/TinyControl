module TinyControl.Server
  ( Data(..),
    serveData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as Packet

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

import System.Timeout(timeout)
import Control.Concurrent(myThreadId, forkIO)

import Data.ByteString (ByteString)
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
  , recvFrom
  , bindSocket)
import Network.BSD (HostName, defaultProtocol)
import System.Time (TimeDiff(..), CalendarTime, getClockTime, toCalendarTime)

import Data.Set (Set)
import qualified Data.Set as Set

data ServerState = ServerState { rto :: TimeDiff
                               , tld :: CalendarTime
                               , r :: Maybe TimeDiff
                               , x_recvset :: Set (CalendarTime, Int)
                               , x :: Int
                               }
                               deriving (Show, Read)

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
      myId <- myThreadId
      resp <- f msg
      handle <- send handle friend resp
      close handle

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
       now <- getClockTime
       calNow <- toCalendarTime now
       let theState = ServerState {
            rto = makeTimeDiff 2,
            tld = calNow,
            r = Nothing,
            x_recvset = Set.empty,
            x = Packet.s
            }

       -- Send back the handle
       return $ Handle { sock=theSock, state=theState }


srecv :: ServerHandle -> IO (ServerHandle, Friend, Data)
srecv h = C.srecv h recieveHelper
  where
    recieveHelper :: ServerStateMonad (Socket) (Friend, Data) -- t m a
    recieveHelper = do
      sock <- ask
      -- Receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
      (msg, num, addr) <- lift (recvFrom sock 1024)
      tell (["Recv'd: " ++ msg])
      let d = (pack msg)
      return (addr, d)


recv :: ServerHandle -> IO (ServerHandle, Friend, Data)
recv h = C.recv h recieveHelper
  where
    recieveHelper :: ServerStateMonad (Socket) (Friend, Data) -- t m a
    recieveHelper = do
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


sendHelper :: ServerStateMonad (Socket,Friend,Data) ()
sendHelper = do
    (sock, friend, d) <- ask
    let msg = unpack d
    lift $ C.sendstr sock friend msg
    tell (["Send'd: " ++ msg])

send :: ServerHandle -> Friend -> Data -> IO (ServerHandle)
send h friend msg = C.send h friend msg sendHelper

close :: ServerHandle -> IO ()
close = C.close
