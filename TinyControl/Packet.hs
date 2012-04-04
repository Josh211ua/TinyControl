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

-- s = payload size
s::Int
s = 1000

dataPacketSize :: Int
dataPacketSize = 1012

isLastDataPacket :: DataPacket -> Bool
isLastDataPacket p = (B.length $ payload p) < s
