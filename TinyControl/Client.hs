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
import System.Time (TimeDiff(..), CalendarTime, getClockTime, toCalendarTime)
import System.Timeout(timeout)
import Data.Set (Set)
import qualified Data.Set as Set

data ClientState = ClientState { rto :: TimeDiff
                               , tld :: CalendarTime
                               , r :: Maybe TimeDiff
                               , x_recvset :: Set (CalendarTime, Int)
                               , x :: Int
                               }
                               deriving (Show, Read)

type ClientHandle = Handle ClientState

type ClientStateMonad r = RWST r ByteString ClientState IO

wantData :: String -> String -> Data -> IO Data
wantData host port msg = do
    x <- open host port
    let (h@(Handle{sock = s, state = ss}), friend) = x
    send s friend (unpack msg)
    recvAll ( h ) friend

recvAll :: ClientHandle -> Friend -> IO Data
recvAll (Handle {sock = s, state = ss}) friend = do
    (_, _, w) <- runRWST (m1 (timeout (getTimeout ss) (recv s))) (s, friend) ss
    return w

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


open :: String -> String -> IO (Handle ClientState, Friend)
open hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       theSock <- socket (addrFamily serveraddr) Datagram defaultProtocol
       --let friend = serveraddr

       -- Initailize State
       now <- getClockTime
       calNow <- toCalendarTime now
       let theState = ClientState {
            rto = makeTimeDiff 2,
            tld = calNow,
            r = Nothing,
            x_recvset = Set.empty,
            x = Packet.s
            }

       -- Send back the handle
       return $ (Handle { sock=theSock, state=theState }, addrAddress serveraddr)

recv :: Socket -> IO (Friend, String)
recv sock = do
    (msg, _, addr) <- recvFrom sock 1024
    return (addr, msg)

-- recieveHelper :: ClientStateMonad (Socket) (Friend, Data) -- t m a
-- recieveHelper = do
--   sock <- ask
--   -- Receive one UDP packet, maximum length 1024 bytes,
--   -- and save its content into msg and its source
--   -- IP and port into addr
--   (msg, _, addr) <- lift (recvFrom sock 1024)
--   let d = (pack msg)
--   return (addr, d)
-- 
-- recv :: Handle ClientState -> IO (Handle ClientState, Friend, Data)
-- recv h = C.recv h recieveHelper

send :: Socket -> Friend -> String -> IO ()
send sock friend msg = C.sendstr sock friend msg

-- sendHelper :: ClientStateMonad (Socket,Friend,Data) ()
-- sendHelper = do
--     (sock, friend, msg) <- ask
--     lift $ C.sendstr sock friend (unpack msg)
-- 
-- send :: Handle ClientState -> Friend -> Data -> IO (Handle ClientState)
-- send h friend msg = C.send h friend msg sendHelper

close :: Handle ClientState -> IO ()
close = C.close
