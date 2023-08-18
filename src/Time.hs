{-# LANGUAGE OverloadedRecordDot #-}

module Time where

import qualified Config
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TF
import qualified Data.Time.Format.ISO8601 as ISO8601

data Time = Time
  { format :: Format,
    unixPrecision :: UNIXPrecision
  }

type UNIXTime = POSIX.POSIXTime

data UNIXPrecision
  = MS
  | S
  deriving (Show)

data Format
  = UNIX UNIXPrecision
  | ISO8601
  | Custom String
  deriving (Show)

newTime :: Config.Config -> Time
newTime config = Time format unixPrecision
  where
    unixPrecision = case Config.precision =<< config.unixConfig of
      Nothing -> Time.MS
      Just Config.MS -> Time.MS
      Just Config.S -> Time.S

    format = case config.format of
      Nothing -> Time.UNIX unixPrecision
      Just Config.UNIX -> Time.UNIX unixPrecision
      Just Config.ISO8601 -> Time.ISO8601
      Just (Config.Custom f) -> Time.Custom f

toText :: Format -> UNIXTime -> T.Text
toText (UNIX S) = T.pack . show . (round :: (POSIX.POSIXTime -> Integer))
toText (UNIX MS) = toText (UNIX S) . (* 1000)
toText ISO8601 =
  T.pack
    . ISO8601.iso8601Show
    . POSIX.posixSecondsToUTCTime
toText (Custom f) =
  T.pack
    . TF.formatTime TF.defaultTimeLocale f
    . POSIX.posixSecondsToUTCTime

data Unit
  = Millisecond
  | Second
  | Minute
  | Hour
  | Day
  deriving (Show)

unixCoeff :: Unit -> UNIXTime
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

intervalToUnixCoeff :: Interval -> UNIXTime
intervalToUnixCoeff (Interval unit coeff) = unixCoeff unit * realToFrac coeff

elapse :: [Interval] -> UNIXTime -> UNIXTime
elapse = (+) . sum . map intervalToUnixCoeff

now :: IO POSIX.POSIXTime
now = POSIX.getPOSIXTime

range :: UNIXTime -> Integer -> [Interval] -> [UNIXTime]
range ts n timeUnits = take (fromIntegral n) [ts, ts + step ..]
  where
    step = sum . map intervalToUnixCoeff $ timeUnits
