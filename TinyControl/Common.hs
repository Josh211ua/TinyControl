module TinyControl.Common
  ( Data(..)
  ,  Friend
  , Handle(..)
  , srecv
  , recv
  , send
  , close
  , makeTimeDiff
  , sendstr
  ) where

import qualified TinyControl.Packet as Packet

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

import Data.ByteString (ByteString)
import Network.Socket (
  Socket
  , SockAddr
  , getAddrInfo
  , socket
  , sendTo
  , SocketType(Datagram)
  , withSocketsDo
  , sClose
  , AddrInfo(..)
  , AddrInfoFlag(AI_PASSIVE)
  , defaultHints)
import Network.BSD (HostName, defaultProtocol)
import System.Time (TimeDiff(..), CalendarTime, getClockTime, toCalendarTime)

import Data.Set (Set)
import Data.List (genericDrop)
import qualified Data.Set as Set

type Data = ByteString

data Handle s = Handle { sock :: Socket
                       , state :: s
                       }
                       deriving (Show)

type Friend = SockAddr

makeTimeDiff :: Int -> TimeDiff
makeTimeDiff sec =
  TimeDiff {
    tdYear = 0,
    tdMonth = 0,
    tdDay = 0,
    tdHour = 0,
    tdMin = 0,
    tdSec = sec,
    tdPicosec = 0
  }

type MyRWST r s a = RWST r [String] s IO a

srecv ::(Show s) => Handle s -> MyRWST (Socket) s (Friend, Data) -> IO (Handle s, Friend, Data)
srecv (Handle {sock = s , state = ss}) helper  = --error "recv not implemented"
  withSocketsDo $
  do
    result <- runRWST helper s ss
    print $ show $ result
    let ((friend, val), state,_) = result
    addrinfos <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing Nothing
    let serveraddr = head addrinfos
     -- Establish a socket for communication
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    return $ (Handle {sock = sock, state = state}, friend, val)
    -- Change addr to a new port

recv ::(Show s) => Handle s -> MyRWST (Socket) s (Friend, Data) -> IO (Handle s, Friend, Data)
recv (Handle {sock = a , state = ss}) helper = --error "recv not implemented"
  withSocketsDo $
  do
    result <- runRWST helper a ss
    print $ show $ result
    let ((friend, val), state,_) = result
    return $ (Handle {sock = a, state = state}, friend, val)

send ::(Show s) => Handle s -> Friend -> Data -> MyRWST (Socket,Friend,Data) s () -> IO (Handle s)
send (Handle {sock = a , state = ss}) friend msg helper =
  withSocketsDo $
  do
    result <- runRWST helper (a,friend,msg) ss
    print $ show $ result
    let (_, state,_) = result
    return $ (Handle {sock = a, state = state})

sendstr :: Socket -> Friend -> String -> IO ()
sendstr _ _ [] = return ()
sendstr sock friend omsg = do sent <- sendTo sock omsg friend
                              sendstr sock friend (genericDrop sent omsg)


close :: Handle s -> IO ()
close (Handle {sock = s , state = _}) = sClose (s)
