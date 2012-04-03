module TinyControl.Client
  ( Data(..)
  , wantData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as Packet

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

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

type ClientStateMonad r = RWST r [String] ClientState IO

wantData :: String -> String -> Data -> IO Data
wantData host port msg = do
    x <- open host port
    let (h@(Handle{sock = s, state = ss}), friend) = x
    send s friend msg
    recvAll ( h ) friend

recvAll :: ClientHandle -> Friend -> IO Data
recvAll (Handle {sock = s, state = ss}) friend = do
    (d, _, _) <- runRWST (m1 (timeout 1 (recv s))) (s, friend) ss
    return d

m1 :: IO (Maybe (Friend, Data)) -> ClientStateMonad (Socket, Friend) (Data)
m1 result = do
    d <- lift result
    error "fail"
        
--    m1 (handle, friend)
--    where
--        m1 :: ClientStateMonad (Socket, Friend)
--        m1 = do
--            (handle, friend) <- ask
--            maybe <- timeout (undefined) $ recv handle
--            case maybe of
--                 Just(h, f, msg) -> -- doSomething
--                    m2
--                 None -> -- doSomething
--                    m3
--        m2 :: ClientStateMonad (Socket, Friend)
--        m2 = do
--
--        m3 :: ClientStateMonad (Socket, Friend)
--        m3 = do
--            (handle, friend) <- ask
--            (h, f, msg) <- recv handle
--            -- Do Something
--            m1 (h, f)

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

recv :: Socket -> IO (Friend, Data)
recv sock = do
    (msg, _, addr) <- recvFrom sock 1024
    let d = (pack msg)
    return (addr, d)

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

send :: Socket -> Friend -> Data -> IO ()
send sock friend msg = C.sendstr sock friend (unpack msg)

-- sendHelper :: ClientStateMonad (Socket,Friend,Data) ()
-- sendHelper = do
--     (sock, friend, msg) <- ask
--     lift $ C.sendstr sock friend (unpack msg)
-- 
-- send :: Handle ClientState -> Friend -> Data -> IO (Handle ClientState)
-- send h friend msg = C.send h friend msg sendHelper

close :: Handle ClientState -> IO ()
close = C.close
