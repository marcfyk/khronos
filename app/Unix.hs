module Unix where

import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format.ISO8601 as ISO8601

type Unix = POSIX.POSIXTime

data Format = Unix | ISO8601

elapse :: Unix -> IO Unix
elapse offset = (offset +) <$> POSIX.getPOSIXTime

now :: IO POSIX.POSIXTime
now = elapse 0

toISO8601String :: Unix -> String
toISO8601String = ISO8601.iso8601Show . POSIX.posixSecondsToUTCTime

toUnixString :: Unix -> String
toUnixString = show . (round :: (POSIX.POSIXTime -> Integer))
