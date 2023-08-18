{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import qualified Config
import qualified Data.Char as Time
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
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
import qualified Out
import qualified Time

data App = App
  { configPath :: Either Config.NoConfig FilePath,
    config :: Config.Config,
    time :: Time.Time,
    out :: Out.Out
  }

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
  (configPath, config) <- Config.load
  let app = newApp configPath config
  opts <- runParser
  let command = optCommand opts
  runCommands app command

newApp :: Either Config.NoConfig FilePath -> Config.Config -> App
newApp configPath config = App configPath config time out
  where
    time = Time.newTime config
    out = Out.newOut

runParser :: IO Opts
runParser = execParser optsParser
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

runCommands :: App -> Command -> IO ()
runCommands app Env = TIO.putStrLn toDisplay
  where
    configPathDisplay :: TLB.Builder
    configPathDisplay = case app.configPath of
      Left err -> TLB.fromString . show $ err
      Right fp -> TLB.fromString $ "using config file at: " <> fp

    configFormat :: TLB.Builder
    configFormat = TLB.fromString $ case app.config.format of
      Nothing -> "format: " <> show Config.defaultFormat <> "(not set, using defaults)"
      Just f -> "format: " <> show f

    configUNIXConfig :: TLB.Builder
    configUNIXConfig = unixConfigDisplay
      where
        header :: Maybe Config.UNIXConfig -> TLB.Builder
        header Nothing = "UNIX config (MISSING):"
        header (Just unix) = "UNIX config: "

        precision :: Maybe Config.UNIXPrecision -> TLB.Builder
        precision Nothing = "precision: " <> p <> "(not set, using defaults)"
          where
            p = TLB.fromString . show $ Config.defaultUNIXPrecision
        precision (Just p) = "precision: " <> (TLB.fromString . show $ p)

        headerDisplay = header app.config.unixConfig
        precisionDisplay = precision (Config.precision =<< app.config.unixConfig)
        unixConfigDisplay = foldr1 (\acc t -> acc <> "\n" <> t) [headerDisplay, precisionDisplay]

    toDisplay = TL.toStrict . TLB.toLazyText $ foldr1 (\acc t -> acc <> "\n" <> t) [configPathDisplay, configFormat, configUNIXConfig]
runCommands app (Now format) = TIO.putStrLn . Time.toText app.time.format =<< Time.now
runCommands app (Elapse format timestamp interval) =
  TIO.putStrLn
    . Time.toText app.time.format
    . Time.elapse (convertInterval interval)
    =<< Time.now
runCommands app (Range format timestamp (Take n) interval) = do
  ts <- timeStampToUNIXOrNow timestamp
  let tsRange = Time.range ts n . convertInterval $ interval
  TIO.putStrLn . T.intercalate "\n" . map (Time.toText app.time.format) $ tsRange

timeStampToUNIX :: TimeStamp -> IO Time.UNIXTime
timeStampToUNIX (TSUNIX ts) = return . realToFrac $ ts
timeStampToUNIX TSNow = Time.now

timeStampToUNIXOrNow :: Maybe TimeStamp -> IO Time.UNIXTime
timeStampToUNIXOrNow = Maybe.maybe Time.now timeStampToUNIX

convertInterval :: Interval -> [Time.Interval]
convertInterval (Interval ms s m h d) =
  zipWith
    Time.Interval
    [Time.Millisecond, Time.Second, Time.Minute, Time.Hour, Time.Day]
    (map (Maybe.fromMaybe 0) [ms, s, m, h, d])
