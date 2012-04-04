module TinyControl.Client
  ( Data(..)
  , wantData
  ) where

import TinyControl.Common (Handle(..), Data(..), Friend, makeTimeDiff)
import qualified TinyControl.Common as C
import qualified TinyControl.Packet as P
import TinyControl.Packet (DataPacket)
import TinyControl.Time (diffTimeToUs)

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
import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Set (Set)
import qualified Data.Set as Set

data ClientState = ClientState { lastDataPacket :: P.DataPacket
                               , lastDataPacketTime :: UTCTime
                               , nextTimeout :: Maybe UTCTime
                               , p :: Float
                               , x_recv :: Int
                               }
                               deriving (Show, Read)

type ClientStateMonad r = RWST r ByteString ClientState IO

wantData :: String -> String -> Data -> IO Data
wantData host port msg = do
    (sock, friend) <- open host port
    send sock friend (unpack msg)
    firstPacket sock

firstPacket :: Socket -> IO Data
firstPacket sock = do
    (msg, _, friend) <- recvFrom sock P.dataPacketSize
    t_delay_start <- getCurrentTime
    -- read data packet
    let packet = read msg
    if P.isLastDataPacket packet
       then return $ P.payload packet
       else do
         -- initialize state from pack
         let ss = initialState packet t_delay_start
         -- send Feedback Packet
         makeAndSendFeedbackPacket sock friend ss
         -- determine timeout interval
         -- begin running through states
         (_, _, w) <- runRWST (m1 (timeout (getTimeout ss) (recv sock))) (sock, friend) ss
         return $ (P.payload packet) `append` w

initialState :: P.DataPacket -> UTCTime -> ClientState
initialState dp t_delay_start = ClientState { 
     lastDataPacket = dp
   , lastDataPacketTime = t_delay_start
   , nextTimeout = Nothing
   , p = 0
   , x_recv = 0
   }

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
             lift $ makeAndSendFeedbackPacket sock f ss
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
             lift $ makeAndSendFeedbackPacket sock f ss
             m1 (timeout (getTimeout ss) (recv sock))
         Nothing -> do
             expireFeedbackTimer
             m3 (timeout (getTimeout ss) (recv sock))

getTimeout :: ClientState -> Int
getTimeout = error "getTimeout not implemented"

makeAndSendFeedbackPacket :: Socket -> Friend -> ClientState -> IO ()
makeAndSendFeedbackPacket sock friend ss = do
    feedbackPacket <- makeFeedbackPacket ss
    sendFeedbackPacket sock friend feedbackPacket

makeFeedbackPacket :: ClientState -> IO P.FeedbackPacket
makeFeedbackPacket s = do
    now <- getCurrentTime
    return P.FeedbackPacket {
        P.t_recvdata = (P.timeStamp (lastDataPacket s))
      , P.t_delay = diffTimeToUs $ now `diffUTCTime` (lastDataPacketTime s)
      , P.x_recv = x_recv s
      , P.p = p s }

sendFeedbackPacket :: Socket -> Friend -> P.FeedbackPacket -> IO ()
sendFeedbackPacket sock friend packet = do
    -- TODO cut out middle man, this should always be exactly 16 bytes once
    --  we are sending correctly
    send sock friend (show packet)

gotDataPacket :: DataPacket -> ClientStateMonad (Socket, Friend) ()
gotDataPacket p = do
    tell (P.payload p)
    return ()

expireFeedbackTimer :: ClientStateMonad (Socket, Friend) ()
expireFeedbackTimer = do return ()

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
