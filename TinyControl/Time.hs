module TinyControl.Time
  ( diffTimeToS
  , sToDiffTime
  , nextTimeout
  , getTimeout
  , now
  , toUs
  ) where

import Data.Time

now :: IO UTCTime
now = getCurrentTime

-- Converts seconds into microseconds
toUs :: Int -> Int
toUs s = 1000 * 1000 * (fromIntegral s)

diffTimeToS :: NominalDiffTime -> Int
diffTimeToS diff = floor $ toRational $ diff

sToDiffTime :: Int -> NominalDiffTime
sToDiffTime s = realToFrac $ secondsToDiffTime (fromIntegral s)

-- Returns a time us seconds after now
nextTimeout :: Int -> IO UTCTime
nextTimeout s = do
    now <- getCurrentTime
    let diff = secondsToDiffTime $ fromIntegral s
    let next = addUTCTime (realToFrac diff) now
    return next

getTimeout :: UTCTime -> IO Int
getTimeout t = do
    now <- getCurrentTime
    let diff = t `diffUTCTime` now
    let realDiff = floor $ (toRational diff)
    if realDiff < 0
       then return 0
       else return realDiff
