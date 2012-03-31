import System.Time (ClockTime, TimeDiff, CalendarTime)
import Data.ByteString (ByteString)

data DataPacket = DataPacket { seqNum :: Int
                             , timeStamp :: CalendarTime
                             , rtt :: TimeDiff
                             , payload :: [ByteString] 
                             } deriving (Show, Read)

data FeedbackPacket = FeedbackPacket { t_recvdata :: CalendarTime
                                     , t_delay :: TimeDiff
                                     , x_recv :: Int
                                     , p :: Float
                                     } deriving (Show, Read)
