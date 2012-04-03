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
    (handle, friend) <- open host port
    handle' <- send handle friend msg
    recvAll handle' friend

recvAll :: ClientHandle -> Friend -> IO Data
recvAll handle friend = error "recvAll not initialized"



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

recieveHelper :: ClientStateMonad (Socket) (Friend, Data) -- t m a
recieveHelper = do
  sock <- ask
  -- Receive one UDP packet, maximum length 1024 bytes,
  -- and save its content into msg and its source
  -- IP and port into addr
  (msg, _, addr) <- lift (recvFrom sock 1024)
  let d = (pack msg)
  return (addr, d)

recv :: Handle ClientState -> IO (Handle ClientState, Friend, Data)
recv h = C.recv h recieveHelper

sendHelper :: ClientStateMonad (Socket,Friend,Data) ()
sendHelper = do
    (sock, friend, msg) <- ask
    lift $ C.sendstr sock friend (unpack msg)

send :: Handle ClientState -> Friend -> Data -> IO (Handle ClientState)
send h friend msg = C.send h friend msg sendHelper

close :: Handle ClientState -> IO ()
close = C.close
