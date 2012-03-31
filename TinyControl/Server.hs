module TinyControl.Server
  ( Data(..)
  , Handle
  , open
  , recv
  , send
  , close
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

data Addr = Addr { sock :: Socket
                 , address :: SockAddr
                 }
                 deriving (Show)

data ServerState = ServerState { rto :: TimeDiff
                               , tld :: CalendarTime
                               , r :: Maybe TimeDiff
                               , x_recvset :: Set (CalendarTime, Int)
                               , x :: Int
                               }
                               deriving (Show, Read)

data Handle = Handle { addr :: Addr
                     , state :: ServerState
                     }
                     deriving (Show)

type ServerStateMonad = RWST Int [String] ServerState IO


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

-- Hostname -> Port -> Handle
open :: String -> IO Handle
--open = error "open not implemented"
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

       -- Save information into an Addr
       let theAddr = Addr { sock=theSock, address=addrAddress serveraddr }

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
       return $ Handle { addr=theAddr, state=theState }


recv :: Handle -> IO (Handle, Data)
recv (Handle {addr = a , state = ss})  = --error "recv not implemented"
  withSocketsDo $
  do
    result <- runRWST helper 0 ss
    print $ show $ result
    let (val, state,_) = result
    return $ (Handle {addr = a, state = state}, val)
  where
    helper :: ServerStateMonad (Data) -- t m a
    helper = error "recv not implemented"

send :: Handle -> Data -> IO (Handle)
send (Handle {addr = a , state = ss}) msg = --error "send not implemented"
  withSocketsDo $
  do
    result <- runRWST helper 0 ss
    print $ show $ result
    let (_, state,_) = result
    return $ (Handle {addr = a, state = state})
  where
    helper :: ServerStateMonad () -- t m a
    helper = error "send not implemented"


close :: Handle -> IO ()
close (Handle {addr = Addr { sock = s, address = _} , state = _}) = sClose (s)
