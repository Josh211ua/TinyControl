module TinyControl.Server
  ( Data(..)
  , Handle
  , open
  , recv
  , send
  , close
  ) where

import qualified TinyControl.Packet as Packet

import Data.ByteString (ByteString)
import Network.Socket (Socket, SockAddr, getAddrInfo, socket, addrFamily, SocketType(Datagram), addrAddress)
import Network.BSD (HostName, defaultProtocol)
import System.Time (TimeDiff(..), CalendarTime, getClockTime, toCalendarTime)

import Data.Set (Set)
import qualified Data.Set as Set

type Data = [ByteString]

data Addr = Addr { sock :: Socket
                 , address :: SockAddr
                 }

data ServerState = ServerState { rto :: TimeDiff
                               , tld :: CalendarTime
                               , r :: Maybe TimeDiff
                               , x_recvset :: Set (CalendarTime, Int)
                               , x :: Int
                               }

data Handle = Handle { addr :: Addr
                     , state :: ServerState
                     }

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
open :: HostName -> String -> IO Handle
--open = error "open not implemented"
open hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
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
recv handle = error "recv not implemented"

send :: Handle -> Data -> IO (Handle)
send handle msg = error "send not implemented"

close :: Handle -> IO ()
close handle = error "close not implemented"
