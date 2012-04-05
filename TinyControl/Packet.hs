module TinyControl.Packet
    (
          DataPacket(..)
        , FeedbackPacket(..)
        , s
        , dataPacketSize
        , isLastDataPacket)
    where
import Data.List(replicate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Time (UTCTime)
import Data.Binary (decode, encode)
import Debug.Trace (trace)

data DataPacket = DataPacket { seqNum :: Int
                             , timeStamp :: UTCTime
                             , rtt :: Int
                             , payload :: ByteString
                             }

defaultPacket = DataPacket { seqNum = 0
                            , timeStamp = read "0000-00-00 00:00:00"
                            , rtt = 1
                            , payload = BC.pack (replicate 1000 'm')
                            }

instance Show DataPacket where
    show DataPacket { seqNum = s
                    , timeStamp = t
                    , rtt = r
                    , payload = p } =
        BLC.unpack ( encode (s, r, p) )

instance Read DataPacket where
    readsPrec _ b =
        let (s, r, p) = decode $ BLC.pack $ b in
        [(DataPacket { seqNum = s
                   , timeStamp = read "0000-00-00 00:00:00"
                   , rtt = r
                   , payload = p }, "")]

data FeedbackPacket = FeedbackPacket { t_recvdata :: UTCTime
                                     , t_delay :: Int
                                     , x_recv :: Int
                                     , p :: Float
                                     }

instance Show FeedbackPacket where
    show FeedbackPacket { t_recvdata = r
                        , t_delay = d
                        , x_recv = x
                        , p = l} =
        BLC.unpack ( encode (d, x, l) )

instance Read FeedbackPacket where
    readsPrec _ b =
        let (d, x, l) = decode $ BLC.pack $ b in
        [(FeedbackPacket { t_recvdata = read "0000-00-00 00:00:00"
                         , t_delay = d
                         , x_recv = x
                         , p = l}, "")]

-- s = payload size
s::Int
s = 1000

dataPacketSize :: Int
dataPacketSize = 1008 + 8 + 8 --payload + encoding payload + int + float

isLastDataPacket :: DataPacket -> Bool
isLastDataPacket p = (B.length $ payload p) < s
