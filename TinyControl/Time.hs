module TinyControl.Time
  ( diffTimeToUs
  , usToDiffTime
  , nextTimeout
  , getTimeout
  , now
  ) where

import Data.Time.Clock

now :: IO UTCTime
now = getCurrentTime

-- Converts seconds into microseconds
toUs :: Int -> Integer
toUs s = 1000 * 1000 * (fromIntegral s)

diffTimeToUs :: NominalDiffTime -> Int
diffTimeToUs diff = do
    floor $ (toRational diff) * 1000 * 1000

usToDiffTime :: Int -> NominalDiffTime
usToDiffTime us = do
    realToFrac $ picosecondsToDiffTime ((fromIntegral us) * 1000 * 1000)

-- Returns a time us microseconds after now
nextTimeout :: Integer -> IO UTCTime
nextTimeout us = do
    now <- getCurrentTime
    let diff = picosecondsToDiffTime (us * 1000 * 1000)
    let next = addUTCTime (realToFrac diff) now
    return next

getTimeout :: UTCTime -> IO Int
getTimeout t = do
    now <- getCurrentTime
    let diff = now `diffUTCTime` t
    let realDiff = floor $ (toRational diff) * 1000 * 1000
    if realDiff < 0
       then return 0
       else return realDiff
