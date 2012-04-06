module TinyControl.Time
  ( diffTimeToS
  , sToDiffTime
  , nextTimeoutNDT
  , nextTimeoutSec
  , nextTimeoutSecPure
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

nextTimeoutNDT :: NominalDiffTime -> IO UTCTime
nextTimeoutNDT ndt = do
  currentTime <- now
  return (addUTCTime ndt currentTime)

-- Returns a time s seconds after now
nextTimeoutSec :: Int -> IO UTCTime
nextTimeoutSec s = do
    now <- getCurrentTime
    return $ nextTimeoutSecPure s now

nextTimeoutSecPure :: Int -> UTCTime -> UTCTime
nextTimeoutSecPure s now =
    let diff = secondsToDiffTime $ fromIntegral s in
    let next = addUTCTime (realToFrac diff) now in
    next



-- number of seconds from now to given time
getTimeout :: UTCTime -> IO Int
getTimeout t = do
    now <- getCurrentTime
    let diff = t `diffUTCTime` now
    let realDiff = floor $ (toRational diff)
    if realDiff < 0
       then return 0
       else return realDiff
