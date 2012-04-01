module TinyControl.Server
  ( Data(..)
  ,  Friend
  , Handle
  , open
  , srecv
  , recv
  , send
  , close
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as Packet

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy hiding (state)

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

data ServerState = ServerState { rto :: TimeDiff
                               , tld :: CalendarTime
                               , r :: Maybe TimeDiff
                               , x_recvset :: Set (CalendarTime, Int)
                               , x :: Int
                               }
                               deriving (Show, Read)

type ServerStateMonad r = RWST r [String] ServerState IO


open :: String -> IO (Handle ServerState)
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

recieveHelper :: ServerStateMonad (Socket) (Friend, Data) -- t m a
recieveHelper = error "srecv not implemented"

srecv :: Handle ServerState -> IO (Handle ServerState, Friend, Data)
srecv h = C.srecv h recieveHelper

recv :: Handle ServerState -> IO (Handle ServerState, Friend, Data)
recv h = C.recv h recieveHelper

send :: Handle ServerState -> Friend -> Data -> IO (Handle ServerState)
send h friend msg = C.send h friend msg (error "send not implemented")

close :: Handle ServerState -> IO ()
close = C.close
