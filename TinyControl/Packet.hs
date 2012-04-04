module TinyControl.Packet
    (
          DataPacket(..)
        , FeedbackPacket(..)
        , s
        , dataPacketSize)
    where

import System.Time (ClockTime, TimeDiff, CalendarTime)
import Data.ByteString (ByteString)

data DataPacket = DataPacket { seqNum :: Int
                             , timeStamp :: CalendarTime
                             , rtt :: TimeDiff
                             , payload :: ByteString
                             } deriving (Show, Read)

data FeedbackPacket = FeedbackPacket { t_recvdata :: CalendarTime
                                     , t_delay :: TimeDiff
                                     , x_recv :: Int
                                     , p :: Float
                                     } deriving (Show, Read)

s::Int
s = 1000

dataPacketSize :: Int
dataPacketSize = 1012
