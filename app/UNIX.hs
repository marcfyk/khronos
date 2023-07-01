module UNIX where

import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format.ISO8601 as ISO8601

type UNIXTime = POSIX.POSIXTime

data Format = Unix | ISO8601

elapse :: UNIXTime -> IO UNIXTime
elapse offset = (offset +) <$> POSIX.getPOSIXTime

now :: IO POSIX.POSIXTime
now = elapse 0

toISO8601String :: UNIXTime -> String
toISO8601String = ISO8601.iso8601Show . POSIX.posixSecondsToUTCTime

toUNIXString :: UNIXTime -> String
toUNIXString = show . (round :: (POSIX.POSIXTime -> Integer))
