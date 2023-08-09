module Time where

import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format.ISO8601 as ISO8601

type UnixTime = POSIX.POSIXTime

data UNIXPrecision = MS | S deriving (Show)

data Format
  = UNIX UNIXPrecision
  | ISO8601
  deriving (Show)

toText :: Format -> UnixTime -> T.Text
toText (UNIX S) = T.pack . show . (round :: (POSIX.POSIXTime -> Integer))
toText (UNIX MS) = toText (UNIX S) . (* 1000)
toText ISO8601 = T.pack . ISO8601.iso8601Show . POSIX.posixSecondsToUTCTime

data Unit
  = Millisecond
  | Second
  | Minute
  | Hour
  | Day
  deriving (Show)

unixCoeff :: Unit -> UnixTime
unixCoeff Millisecond = 0.001
unixCoeff Second = 1000 * unixCoeff Millisecond
unixCoeff Minute = 60 * unixCoeff Second
unixCoeff Hour = 60 * unixCoeff Minute
unixCoeff Day = 24 * unixCoeff Hour

data Interval = Interval
  { unit :: Unit,
    coeff :: Integer
  }
  deriving (Show)

intervalToUnixCoeff :: Interval -> UnixTime
intervalToUnixCoeff (Interval unit coeff) = unixCoeff unit * realToFrac coeff

elapse :: [Interval] -> UnixTime -> UnixTime
elapse = (+) . sum . map intervalToUnixCoeff

now :: IO POSIX.POSIXTime
now = POSIX.getPOSIXTime

range :: UnixTime -> Integer -> [Interval] -> [UnixTime]
range ts n timeUnits = take (fromIntegral n) [ts, ts + step ..]
  where
    step = sum . map intervalToUnixCoeff $ timeUnits
