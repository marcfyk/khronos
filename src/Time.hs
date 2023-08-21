{-# LANGUAGE OverloadedRecordDot #-}

module Time where

import qualified Config
import qualified Config as Time
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TF
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Time.LocalTime as LocalTime

data Time = Time
  { format :: Format,
    tz :: Time.TimeZone,
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
  | Custom String Time.TimeZone
  deriving (Show)

newTime :: Either Config.NoConfig (FilePath, Config.Config) -> IO Time
newTime (Left _) = do
  tz <- Time.getCurrentTimeZone
  return $ Time format tz unixPrecision
  where
    unixPrecision = MS
    format = UNIX MS
newTime (Right (_, config)) = do
  tz <- getTZ
  let unixPrecision = getUNIXPrecision (config.unixConfig >>= Config.precision)
  let format = getFormat config.format unixPrecision tz
  return $ Time format tz unixPrecision
  where
    getTZ :: IO LocalTime.TimeZone
    getTZ = case config.utc of
      Nothing -> Time.getCurrentTimeZone
      Just n -> return (Time.hoursToTimeZone n)

    getUNIXPrecision :: Maybe Config.UNIXPrecision -> UNIXPrecision
    getUNIXPrecision Nothing = MS
    getUNIXPrecision (Just Config.MS) = MS
    getUNIXPrecision (Just Config.S) = S

    getFormat :: Maybe Config.Format -> UNIXPrecision -> LocalTime.TimeZone -> Format
    getFormat Nothing unixPrecision _ = UNIX unixPrecision
    getFormat (Just Config.UNIX) unixPrecision _ = UNIX unixPrecision
    getFormat (Just Config.ISO8601) _ _ = ISO8601
    getFormat (Just (Config.Custom format)) _ tz = Custom format tz

data ErrFormat = ErrFormat

toText :: Format -> UNIXTime -> T.Text
toText format ts = formatted
  where
    formatted = case format of
      UNIX S -> T.pack . show . round $ ts
      UNIX MS -> T.pack . show . round . (* 1000) $ ts
      ISO8601 -> T.pack . ISO8601.iso8601Show . POSIX.posixSecondsToUTCTime $ ts
      Custom f tz ->
        T.pack
          . TF.formatTime TF.defaultTimeLocale f
          . Time.addUTCTime (fromIntegral offset)
          . POSIX.posixSecondsToUTCTime
          $ ts
        where
          getUTCOffsetSeconds :: Time.TimeZone -> Int
          getUTCOffsetSeconds = (* 60) . Time.timeZoneMinutes
          offset = getUTCOffsetSeconds tz

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
