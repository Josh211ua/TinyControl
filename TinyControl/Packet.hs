module TinyControl.Packet
    (
          DataPacket(..)
        , FeedbackPacket(..)
        , s
        , dataPacketSize
        , isLastDataPacket)
    where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Time (UTCTime)
import Data.Binary (decode, encode)

data DataPacket = DataPacket { seqNum :: Int
                             , timeStamp :: UTCTime
                             , rtt :: Int
                             , payload :: ByteString
                             } 

instance Show DataPacket where
    show DataPacket { seqNum = s
                    , timeStamp = t
                    , rtt = r
                    , payload = p } = 
       show ( encode (s, r, p) )

instance Read DataPacket where
    readsPrec _ b =
        let (s, r, p) = decode $ read $ b in
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
        show ( encode (d, x, l) )

instance Read FeedbackPacket where
    readsPrec _ b = 
        let (d, x, l) = decode $ read $ b in
        [(FeedbackPacket { t_recvdata = read "0000-00-00 00:00:00"
                         , t_delay = d
                         , x_recv = x
                         , p = l}, "")]

-- s = payload size
s::Int
s = 1000

dataPacketSize :: Int
dataPacketSize = 1012

isLastDataPacket :: DataPacket -> Bool
isLastDataPacket p = (B.length $ payload p) < s
