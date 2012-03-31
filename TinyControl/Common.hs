module TinyControl.Common
  ( Data(..)
  ,  Friend
  , Handle(..)
  , srecv
  , recv
  , send
  , close
  , makeTimeDiff
  ) where

import qualified TinyControl.Packet as Packet

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy

import Data.ByteString (ByteString)
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
  , defaultHints)
import Network.BSD (HostName, defaultProtocol)
import System.Time (TimeDiff(..), CalendarTime, getClockTime, toCalendarTime)

import Data.Set (Set)
import qualified Data.Set as Set

type Data = [ByteString]

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

type MyRWST s a = RWST Int [String] s IO a

srecv ::(Show s) => Handle s -> MyRWST s (Friend, Data) -> IO (Handle s, Friend, Data)
srecv (Handle {sock = s , state = ss}) helper  = --error "recv not implemented"
  withSocketsDo $
  do
    result <- runRWST helper 0 ss
    print $ show $ result
    let ((friend, val), state,_) = result
    return $ (Handle {sock = s, state = state}, friend, val)
    -- Change addr to a new port

recv ::(Show s) => Handle s -> MyRWST s (Friend, Data) -> IO (Handle s, Data)
recv (Handle {sock = a , state = ss}) helper = --error "recv not implemented"
  withSocketsDo $
  do
    result <- runRWST helper 0 ss
    print $ show $ result
    let ((_, val), state,_) = result
    return $ (Handle {sock = a, state = state}, val)

send ::(Show s) => Handle s -> Friend -> Data -> MyRWST s (Handle s) -> IO (Handle s)
send (Handle {sock = a , state = ss}) friend msg helper =
  withSocketsDo $
  do
    result <- runRWST helper 0 ss
    print $ show $ result
    let (_, state,_) = result
    return $ (Handle {sock = a, state = state})


close :: Handle s -> IO ()
close (Handle {sock = s , state = _}) = sClose (s)
