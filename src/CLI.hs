{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Data.Char (GeneralCategory (Format))
import qualified Data.Char as Time
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Env
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
    strOption,
    subparser,
    (<**>),
    (<|>),
  )
import qualified Time

newtype Opts = Opts {optCommand :: Command}

data Command
  = Env
  | Now (Maybe Format)
  | Elapse (Maybe Format) (Maybe TimeStamp) Interval
  | Range (Maybe Format) (Maybe TimeStamp) Take Interval
  | Seconds Interval

data Format
  = UNIXS
  | UNIXMS
  | ISO8601
  | Custom String

data Interval = Interval
  { milliseconds :: Maybe Integer,
    seconds :: Maybe Integer,
    minutes :: Maybe Integer,
    hours :: Maybe Integer,
    days :: Maybe Integer
  }

newtype Take = Take Integer

data TimeStamp
  = TSUNIX Integer
  | TSNow

run :: IO ()
run = do
  env <- Env.getEnv
  opts <- runParser env
  let command = optCommand opts
  runCommands env command

runParser :: Env.Env -> IO Opts
runParser env = execParser optsParser
  where
    commandOptions :: Parser Opts
    commandOptions =
      Opts
        <$> subparser
          ( envCommand
              <> nowCommand
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

    envCommand :: Mod CommandFields Command
    envCommand =
      command
        "env"
        ( info
            (envOptions <**> helper)
            (progDesc "Prints the current loaded configuration")
        )
      where
        envOptions :: Parser Command
        envOptions = pure Env

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
            <*> timestampOptions
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

    formatOptions :: Parser (Maybe Format)
    formatOptions = optional $ unixSParser <|> unixMSParser <|> isoParser <|> customParser
      where
        unixSParser :: Parser Format
        unixSParser = flag' UNIXS (long "unix-s" <> help "unix format in seconds")

        unixMSParser :: Parser Format
        unixMSParser = flag' UNIXMS (long "unix-ms" <> help "unix format in milliseconds")

        isoParser :: Parser Format
        isoParser =
          flag'
            ISO8601
            ( long "iso8601"
                <> long "iso"
                <> help "ISO8601 format"
            )

        customParser :: Parser Format
        customParser =
          Custom
            <$> strOption
              ( long "format"
                  <> short 'f'
                  <> metavar "FORMAT"
                  <> help "format for representing time, example: %Y-%m-%d %H:%M:%S"
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
                <> short 'S'
                <> metavar "SECOND_OFFSET"
                <> help "number of seconds relative to the current time"
            )

        minutes :: Parser Integer
        minutes =
          option
            auto
            ( long "minute"
                <> long "min"
                <> short 'M'
                <> metavar "MINUTE_OFFSET"
                <> help "number of minutes relative to the current time"
            )

        hours :: Parser Integer
        hours =
          option
            auto
            ( long "hour"
                <> long "hr"
                <> short 'H'
                <> metavar "HOUR_OFFSET"
                <> help "number of hours relative to the current time"
            )

        days :: Parser Integer
        days =
          option
            auto
            ( long "day"
                <> short 'd'
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

    timestampOptions :: Parser (Maybe TimeStamp)
    timestampOptions = optional $ unixParser <|> nowParser
      where
        unixParser =
          TSUNIX
            <$> option
              auto
              ( long "timestamp"
                  <> long "ts"
                  <> metavar "TIMESTAMP"
                  <> help "timestamp"
              )
        nowParser = flag' TSNow (long "now" <> help "current timestamp")

runCommands :: Env.Env -> Command -> IO ()
runCommands env Env = TIO.putStrLn . Env.displayEnv $ env
runCommands env (Now format) = TIO.putStrLn . formatUNIXSeconds env format =<< Time.now
runCommands env (Elapse format timestamp interval) =
  TIO.putStrLn
    . formatUNIXSeconds env format
    . Time.elapse (convertInterval interval)
    =<< Time.now
runCommands env (Range format timestamp (Take n) interval) = do
  ts <- timeStampToUNIXOrNow timestamp
  let tsRange = Time.range ts n . convertInterval $ interval
  TIO.putStrLn . T.intercalate "\n" . map (formatUNIXSeconds env format) $ tsRange

timeStampToUNIX :: TimeStamp -> IO Time.UNIXTime
timeStampToUNIX (TSUNIX ts) = return . realToFrac $ ts
timeStampToUNIX TSNow = Time.now

timeStampToUNIXOrNow :: Maybe TimeStamp -> IO Time.UNIXTime
timeStampToUNIXOrNow = Maybe.maybe Time.now timeStampToUNIX

formatUNIXSeconds :: Env.Env -> Maybe Format -> Time.UNIXTime -> T.Text
formatUNIXSeconds env format = Time.toText selectedFormat
  where
    selectedFormat = case format of
      Just UNIXMS -> Time.UNIX Time.MS
      Just UNIXS -> Time.UNIX Time.S
      Just ISO8601 -> Time.ISO8601
      Just (Custom f) -> Time.Custom f
      Nothing -> defaultFormat

    defaultPrecision = case env.config.unixConfig.precision of
      Env.MS -> Time.MS
      Env.S -> Time.S

    defaultFormat = case env.config.defaultFormat of
      Env.UNIX -> Time.UNIX defaultPrecision
      Env.ISO8601 -> Time.ISO8601
      Env.Custom f -> Time.Custom f

convertInterval :: Interval -> [Time.Interval]
convertInterval (Interval ms s m h d) =
  zipWith
    Time.Interval
    [Time.Millisecond, Time.Second, Time.Minute, Time.Hour, Time.Day]
    (map (Maybe.fromMaybe 0) [ms, s, m, h, d])
