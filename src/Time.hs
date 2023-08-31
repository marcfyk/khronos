{-# LANGUAGE OverloadedRecordDot #-}

module Time
  ( newTime,
    Time (Time, format, timezone, unixPrecision),
    now,
    toText,
    elapse,
    range,
    UNIXTime,
    Interval (Interval),
    Unit
      ( Millisecond,
        Second,
        Minute,
        Hour,
        Day
      ),
    Format (UNIX, ISO8601, Custom),
    UNIXPrecision (MS, S),
  )
where

import qualified Config
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as TF
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Time.LocalTime as LocalTime

data Time = Time
  { format :: Format,
    timezone :: Time.TimeZone,
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
  return $ Time f tz p
  where
    p = MS
    f = UNIX MS
newTime (Right (_, config)) = do
  tz <- getTZ
  let p = getUNIXPrecision (config.unixConfig >>= Config.precision)
  let f = getFormat config.format p tz
  return $ Time f tz p
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
    getFormat Nothing p _ = UNIX p
    getFormat (Just Config.UNIX) p _ = UNIX p
    getFormat (Just Config.ISO8601) _ _ = ISO8601
    getFormat (Just (Config.Custom f)) _ tz = Custom f tz

toText :: Format -> UNIXTime -> T.Text
toText timeFormat ts = formatted
  where
    formatted = case timeFormat of
      UNIX S -> T.pack . (show :: Integer -> String) . round $ ts
      UNIX MS -> T.pack . (show :: Integer -> String) . round . (* 1000) $ ts
      ISO8601 -> T.pack . ISO8601.iso8601Show . POSIX.posixSecondsToUTCTime $ ts
      Custom f t ->
        T.pack
          . TF.formatTime TF.defaultTimeLocale f
          . Time.addUTCTime (fromIntegral offset)
          . POSIX.posixSecondsToUTCTime
          $ ts
        where
          getUTCOffsetSeconds :: Time.TimeZone -> Int
          getUTCOffsetSeconds = (* 60) . Time.timeZoneMinutes
          offset = getUTCOffsetSeconds t

data Unit
  = Millisecond
  | Second
  | Minute
  | Hour
  | Day
  deriving (Show)

type Coeff = Integer

unixCoeff :: Unit -> UNIXTime
unixCoeff Millisecond = 0.001
unixCoeff Second = 1000 * unixCoeff Millisecond
unixCoeff Minute = 60 * unixCoeff Second
unixCoeff Hour = 60 * unixCoeff Minute
unixCoeff Day = 24 * unixCoeff Hour

data Interval = Interval Unit Coeff deriving (Show)

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
