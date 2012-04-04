module TinyControl.Time
  ( toUs
  , nextTimeout
  , getTimeout
  ) where

import Data.Time.Clock

-- Converts seconds into microseconds
toUs :: Int -> Integer
toUs s = 1000 * 1000 * (fromIntegral s)

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
