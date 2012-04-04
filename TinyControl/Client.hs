module TinyControl.Client
  ( Data(..)
  , wantData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as Packet
import TinyControl.Packet (DataPacket)

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
import Data.Set (Set)
import qualified Data.Set as Set

data ClientState = ClientState { a :: ()
                               }
                               deriving (Show, Read)

type ClientStateMonad r = RWST r ByteString ClientState IO

wantData :: String -> String -> Data -> IO Data
wantData host port msg = do
    (sock, friend) <- open host port
    send sock friend (unpack msg)
    recvAll sock friend

recvAll :: Socket -> Friend -> IO Data
recvAll s friend = do
    let ss = initialState
    (_, _, w) <- runRWST (m1 (timeout (getTimeout ss) (recv s))) (s, friend) ss
    return w

initialState = ClientState { a = () }

m1 :: IO (Maybe (Friend, String)) -> ClientStateMonad (Socket, Friend) ()
m1 result = do
    (sock, f) <- ask
    ss <- get
    maybeD <- lift result
    case (maybeD) of
         Just (_, d) -> do
             let p = read d
             gotDataPacket p
             m2 (timeout (getTimeout ss) (recv sock))
         Nothing -> do
             expireFeedbackTimer
             m3 (timeout (getTimeout ss) (recv sock))
    
m2 :: IO (Maybe (Friend, String)) -> ClientStateMonad (Socket, Friend) ()
m2 result = do
    (sock, f) <- ask
    ss <- get
    maybeD <- lift result
    case (maybeD) of
         Just (_, d) -> do
             let p = read d
             gotDataPacket p
             m2 (timeout (getTimeout ss) (recv sock))
         Nothing -> do
             expireFeedbackTimer
             sendFeedbackPacket
             m3 (timeout (getTimeout ss) (recv sock))

m3 :: IO (Maybe (Friend, String)) -> ClientStateMonad (Socket, Friend) ()
m3 result = do
    (sock, f) <- ask
    ss <- get
    maybeD <- lift result
    case (maybeD) of
         Just (_, d) -> do
             let p = read d
             gotDataPacket p
             m1 (timeout (getTimeout ss) (recv sock))
         Nothing -> do
             expireFeedbackTimer
             m3 (timeout (getTimeout ss) (recv sock))

getTimeout :: ClientState -> Int
getTimeout = error "getTimeout not implemented"

makeDataPacket :: String -> DataPacket
makeDataPacket d = error "makeFeedbackPacket not implemented"

gotDataPacket :: DataPacket -> ClientStateMonad (Socket, Friend) ()
gotDataPacket p = do
    tell (Packet.payload p)
    error "gotDataPacket not implemented"

expireFeedbackTimer :: ClientStateMonad (Socket, Friend) ()
expireFeedbackTimer = error "expireFeedbackTimer not implemented"

sendFeedbackPacket :: ClientStateMonad (Socket, Friend) ()
sendFeedbackPacket = error "sendFeedbackPacket not implemented"


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

recv :: Socket -> IO (Friend, String)
recv sock = do
    (msg, _, addr) <- recvFrom sock 1024
    return (addr, msg)

send :: Socket -> Friend -> String -> IO ()
send sock friend msg = C.sendstr sock friend msg

close :: Socket -> IO ()
close s = sClose s
