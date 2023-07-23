{-# LANGUAGE OverloadedStrings #-}

module CLI where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    ParserInfo,
    auto,
    command,
    execParser,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    subparser,
    (<**>),
    (<|>),
  )
import qualified Time

newtype Opts = Opts {optCommand :: Command}

data Command
  = Now (Maybe Format)
  | Elapse (Maybe Format) Interval
  | Range (Maybe Format) TimeStamp Take Interval
  | Seconds Interval

data Format
  = UnixMS
  | UnixS
  | ISO8601

data Interval = Interval
  { milliseconds :: Maybe Integer,
    seconds :: Maybe Integer,
    minutes :: Maybe Integer,
    hours :: Maybe Integer,
    days :: Maybe Integer
  }

newtype Take = Take Integer

newtype TimeStamp = TimeStamp Integer

run :: IO ()
run = do
  opts <- execParser optsParser
  case optCommand opts of
    Now format -> execNow format
    Elapse format offset -> execElapse format offset
    Range format ts t step -> execRange format ts t step
  where
    commandOptions :: Parser Opts
    commandOptions =
      Opts
        <$> subparser
          ( nowCommand
              <> elapseCommand
              <> rangeCommand
          )

    optsParser :: ParserInfo Opts
    optsParser =
      info
        (commandOptions <**> helper)
        ( fullDesc
            <> progDesc "khronos provides commands to display the result of time operations"
            <> header "khronos - A CLI for time"
        )

    nowCommand :: Mod CommandFields Command
    nowCommand =
      command
        "now"
        ( info
            (nowOptions <**> helper)
            (progDesc "Prints the current time in the specified format (default format is unix)")
        )
      where
        nowOptions :: Parser Command
        nowOptions = Now <$> formatOptions

    elapseCommand :: Mod CommandFields Command
    elapseCommand =
      command
        "elapse"
        ( info
            (elapseOptions <**> helper)
            (progDesc "Prints the elapse time from a given offset in the specified format (default format is unix)")
        )
      where
        elapseOptions :: Parser Command
        elapseOptions =
          Elapse
            <$> formatOptions
            <*> intervalOptions

    rangeCommand :: Mod CommandFields Command
    rangeCommand =
      command
        "range"
        ( info
            (rangeOptions <**> helper)
            (progDesc "Prints a range of time starting from a timestamp (unix seconds) and stepped over with a specific value")
        )
      where
        rangeOptions :: Parser Command
        rangeOptions =
          Range
            <$> formatOptions
            <*> timestampOptions
            <*> takeOptions
            <*> intervalOptions

    execNow :: Maybe Format -> IO ()
    execNow format = TIO.putStrLn . formatUnixSeconds format =<< Time.now

    execElapse :: Maybe Format -> Interval -> IO ()
    execElapse format interval =
      TIO.putStrLn
        . formatUnixSeconds format
        . Time.elapse (convertInterval interval)
        =<< Time.now

    execRange :: Maybe Format -> TimeStamp -> Take -> Interval -> IO ()
    execRange format (TimeStamp ts) (Take n) =
      TIO.putStrLn
        . T.intercalate "\n"
        . map (formatUnixSeconds format)
        . Time.range (realToFrac ts) n
        . convertInterval

    formatUnixSeconds :: Maybe Format -> Time.UnixTime -> T.Text
    formatUnixSeconds Nothing = Time.toText Time.UnixMS
    formatUnixSeconds (Just UnixMS) = Time.toText Time.UnixMS
    formatUnixSeconds (Just UnixS) = Time.toText Time.UnixS
    formatUnixSeconds (Just ISO8601) = Time.toText Time.ISO8601

    convertInterval :: Interval -> [Time.Interval]
    convertInterval (Interval ms s m h d) =
      zipWith
        Time.Interval
        [Time.Millisecond, Time.Second, Time.Minute, Time.Hour, Time.Day]
        (map (Maybe.fromMaybe 0) [s, m, h, d])

formatOptions :: Parser (Maybe Format)
formatOptions =
  optional $
    unixMSFormat
      <|> unixSFormat
      <|> isoFormat
  where
    unixMSFormat :: Parser Format
    unixMSFormat =
      flag'
        UnixMS
        ( long "unix-ms"
            <> long "unix"
            <> help "unix format (milliseconds)"
        )

    unixSFormat :: Parser Format
    unixSFormat =
      flag'
        UnixS
        ( long "unix-s"
            <> help "unix format (seconds)"
        )

    isoFormat :: Parser Format
    isoFormat =
      flag'
        ISO8601
        ( long "iso8601"
            <> long "iso"
            <> help "ISO 8601 format"
        )

intervalOptions :: Parser Interval
intervalOptions =
  Interval
    <$> optional milliseconds
    <*> optional seconds
    <*> optional minutes
    <*> optional hours
    <*> optional days
  where
    milliseconds :: Parser Integer
    milliseconds =
      option
        auto
        ( long "milliseconds"
            <> long "ms"
            <> metavar "MILLISECOND_OFFSET"
            <> help "number of milliseconds relative to the current time"
        )

    seconds :: Parser Integer
    seconds =
      option
        auto
        ( long "second"
            <> long "sec"
            <> metavar "SECOND_OFFSET"
            <> help "number of seconds relative to the current time"
        )

    minutes :: Parser Integer
    minutes =
      option
        auto
        ( long "minute"
            <> long "min"
            <> metavar "MINUTE_OFFSET"
            <> help "number of minutes relative to the current time"
        )

    hours :: Parser Integer
    hours =
      option
        auto
        ( long "hour"
            <> long "hr"
            <> metavar "HOUR_OFFSET"
            <> help "number of hours relative to the current time"
        )

    days :: Parser Integer
    days =
      option
        auto
        ( long "day"
            <> long "dy"
            <> metavar "DAY_OFFSET"
            <> help "number of days relative to the current time"
        )

takeOptions :: Parser Take
takeOptions =
  Take
    <$> option
      auto
      ( long "take"
          <> short 't'
          <> metavar "TAKE_AMOUNT"
          <> help "number of values to take"
      )

timestampOptions :: Parser TimeStamp
timestampOptions =
  TimeStamp
    <$> option
      auto
      ( long "timestamp"
          <> long "ts"
          <> metavar "TIMESTAMP"
          <> help "timestamp"
      )
