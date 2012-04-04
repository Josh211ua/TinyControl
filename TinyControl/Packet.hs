module TinyControl.Packet
    (
          DataPacket(..)
        , FeedbackPacket(..)
        , s
        , dataPacketSize)
    where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)

data DataPacket = DataPacket { seqNum :: Int
                             , timeStamp :: UTCTime
                             , rtt :: Int
                             , payload :: ByteString
                             } deriving (Show, Read)

data FeedbackPacket = FeedbackPacket { t_recvdata :: UTCTime
                                     , t_delay :: Int
                                     , x_recv :: Int
                                     , p :: Float
                                     } deriving (Show, Read)

s::Int
s = 1000

dataPacketSize :: Int
dataPacketSize = 1012
