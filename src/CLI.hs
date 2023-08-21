{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import qualified Config
import qualified Control.Monad as Monad
import qualified Data.Char as Time
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Time
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
import qualified Out.Text
import qualified Text.Printf as Printf
import qualified Time

data App = App
  { configResult :: Either Config.NoConfig (FilePath, Config.Config),
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
  | Custom String (Maybe Int)

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
  configResult <- Config.load
  app <- newApp configResult
  opts <- runParser
  let command = optCommand opts
  runCommands app command

newApp :: Either Config.NoConfig (FilePath, Config.Config) -> IO App
newApp configResult = do
  time <- Time.newTime configResult
  let out = Out.newOut
  return $ App configResult time out

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
    formatOptions =
      optional $
        unixSParser
          <|> unixMSParser
          <|> isoParser
          <|> customParser
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
            <$> formatParser
            <*> utcOffsetParser
          where
            formatParser =
              strOption
                ( long "format"
                    <> short 'f'
                    <> metavar "FORMAT"
                    <> help "format for representing time, example: %Y-%m-%d %H:%M:%S"
                )
            utcOffsetParser =
              optional $
                option
                  auto
                  ( long "utc"
                      <> metavar "UTC_OFFSET"
                      <> help "utc offset to the timestamp"
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
                <> help "number of milliseconds relative to the timestamp"
            )

        seconds :: Parser Integer
        seconds =
          option
            auto
            ( long "second"
                <> long "sec"
                <> short 'S'
                <> metavar "SECOND_OFFSET"
                <> help "number of seconds relative to the timestamp"
            )

        minutes :: Parser Integer
        minutes =
          option
            auto
            ( long "minute"
                <> long "min"
                <> short 'M'
                <> metavar "MINUTE_OFFSET"
                <> help "number of minutes relative to the timestamp"
            )

        hours :: Parser Integer
        hours =
          option
            auto
            ( long "hour"
                <> long "hr"
                <> short 'H'
                <> metavar "HOUR_OFFSET"
                <> help "number of hours relative to the timestamp"
            )

        days :: Parser Integer
        days =
          option
            auto
            ( long "day"
                <> short 'd'
                <> metavar "DAY_OFFSET"
                <> help "number of days relative to the timestamp"
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
runCommands app Env = do
  let filePathResult = fst <$> app.configResult
  let configResult = snd <$> app.configResult
  let unixConfig = Config.unixConfig <$> configResult
  loadedConfigMessage filePathResult
  generalConfigHeader filePathResult
  generalConfigMessage configResult
  unixConfigHeader unixConfig
  unixConfigPrecision ((Config.precision =<<) <$> unixConfig)
  where
    messageNotSetUsingDefaults :: String
    messageNotSetUsingDefaults = "[not set, using defaults]"

    loadedConfigMessage :: Either Config.NoConfig FilePath -> IO ()
    loadedConfigMessage (Left (Config.InvalidFile fp err)) = do
      Out.putStr app.out.error {Out.isUnderlined = True, Out.isBold = True} "Config Status:"
      Out.putStrLn app.out.error (Printf.printf " could not parse config from %s\n%s" fp err)
    loadedConfigMessage (Left Config.NoFile) = do
      Out.putStr app.out.warn {Out.isUnderlined = True, Out.isBold = True} "Config Status:"
      Out.putStrLn app.out.warn " no config file found\n"
    loadedConfigMessage (Right fp) = do
      Out.putStr app.out.info {Out.isUnderlined = True, Out.isBold = True} "Config Status:"
      Out.putStrLn app.out.info $ " using config file at: " <> fp <> "\n"

    generalConfigHeader :: Either Config.NoConfig FilePath -> IO ()
    generalConfigHeader (Right _) =
      Out.putStrLn
        app.out.info {Out.isUnderlined = True, Out.isBold = True}
        "General Config"
    generalConfigHeader (Left _) =
      Out.putStrLn
        app.out.warn {Out.isUnderlined = True, Out.isBold = True}
        $ "General Config " <> messageNotSetUsingDefaults

    generalConfigMessage :: Either Config.NoConfig Config.Config -> IO ()
    generalConfigMessage configResult = do
      formatMessage (Config.format <$> configResult)
      where
        formatMessage :: Either Config.NoConfig (Maybe Config.Format) -> IO ()
        formatMessage (Right (Just f)) =
          Out.putStrLn
            app.out.info
            $ "format: " <> show f <> "\n"
        formatMessage _ =
          Out.putStrLn
            app.out.warn
            $ "format: " <> show Config.defaultFormat <> "(not set, using defaults)\n"

    unixConfigHeader :: Either Config.NoConfig (Maybe Config.UNIXConfig) -> IO ()
    unixConfigHeader (Right (Just unix)) =
      Out.putStrLn
        app.out.info {Out.isUnderlined = True, Out.isBold = True}
        "UNIX Config:"
    unixConfigHeader _ =
      Out.putStrLn
        app.out.warn {Out.isUnderlined = True, Out.isBold = True}
        $ "UNIX config " <> messageNotSetUsingDefaults

    unixConfigPrecision :: Either Config.NoConfig (Maybe Config.UNIXPrecision) -> IO ()
    unixConfigPrecision (Right (Just p)) =
      Out.putStrLn
        app.out.info
        $ "precision: " <> show p <> "\n"
    unixConfigPrecision _ =
      Out.putStrLn
        app.out.warn
        $ "precision: " <> show Config.defaultUNIXPrecision <> " " <> messageNotSetUsingDefaults
runCommands app (Now format) = do
  now <- Time.now
  let format' = parseFormat format app.time.tz
  let formatted = Time.toText format' now
  Out.Text.putStrLn app.out.info formatted
runCommands app (Elapse format timestamp interval) = do
  ts <- timeStampToUNIXOrNow timestamp
  let elapsed = Time.elapse (convertInterval interval) ts
  let format' = parseFormat format app.time.tz
  let formatted = Time.toText format' elapsed
  Out.Text.putStrLn app.out.info formatted
runCommands app (Range format timestamp (Take n) interval) = do
  ts <- timeStampToUNIXOrNow timestamp
  let tsRange = Time.range ts n . convertInterval $ interval
  let format' = parseFormat format app.time.tz
  let formattedTs = map (Time.toText format') tsRange
  let formatted = T.intercalate "\n" formattedTs
  Out.Text.putStrLn app.out.info formatted

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

parseFormat :: Maybe Format -> Data.Time.TimeZone -> Time.Format
parseFormat (Just UNIXMS) _ = Time.UNIX Time.MS
parseFormat (Just UNIXS) _ = Time.UNIX Time.S
parseFormat (Just ISO8601) _ = Time.ISO8601
parseFormat (Just (Custom f (Just offset))) _ = Time.Custom f (Data.Time.minutesToTimeZone (offset * 60))
parseFormat (Just (Custom f Nothing)) tz = Time.Custom f tz
