{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import qualified Config
import qualified Control.Monad as Monad
import qualified Data.Char as Time
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
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
import qualified Out.Text
import qualified Text.Printf as Printf
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
runCommands app Env = do
  loadedConfigMessage app.configPath
  generalConfigHeader app.configPath
  generalConfigMessage app.configPath app.config
  unixConfigHeader app.configPath app.config.unixConfig
  unixConfigPrecision app.configPath (app.config.unixConfig >>= Config.precision)
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

    generalConfigMessage :: Either Config.NoConfig FilePath -> Config.Config -> IO ()
    generalConfigMessage configPath config = do
      formatMessage configPath config.format
      where
        formatMessage :: Either Config.NoConfig FilePath -> Maybe Config.Format -> IO ()
        formatMessage (Right _) (Just f) =
          Out.putStrLn
            app.out.info
            $ "format: " <> show f <> "\n"
        formatMessage (Left _) (Just _) =
          Out.putStrLn
            app.out.warn
            $ "format: " <> show Config.defaultFormat <> "(not set, using defaults)\n"
        formatMessage _ Nothing =
          Out.putStrLn
            app.out.warn
            $ "format: " <> show Config.defaultFormat <> "(not set, using defaults)\n"

    unixConfigHeader :: Either Config.NoConfig FilePath -> Maybe Config.UNIXConfig -> IO ()
    unixConfigHeader (Right _) (Just unix) =
      Out.putStrLn
        app.out.info {Out.isUnderlined = True, Out.isBold = True}
        "UNIX Config:"
    unixConfigHeader (Left _) (Just _) =
      Out.putStrLn
        app.out.warn {Out.isUnderlined = True, Out.isBold = True}
        $ "UNIX config " <> messageNotSetUsingDefaults
    unixConfigHeader _ Nothing =
      Out.putStrLn
        app.out.warn {Out.isUnderlined = True, Out.isBold = True}
        $ "UNIX config " <> messageNotSetUsingDefaults

    unixConfigPrecision :: Either Config.NoConfig FilePath -> Maybe Config.UNIXPrecision -> IO ()
    unixConfigPrecision (Right _) (Just p) =
      Out.putStrLn
        app.out.info
        $ "precision: " <> show p <> "\n"
    unixConfigPrecision (Left _) (Just _) =
      Out.putStrLn
        app.out.warn
        $ "precision: " <> show Config.defaultUNIXPrecision <> " " <> messageNotSetUsingDefaults
    unixConfigPrecision _ Nothing =
      Out.putStrLn
        app.out.warn
        $ "precision: " <> show Config.defaultUNIXPrecision <> " " <> messageNotSetUsingDefaults
runCommands app (Now Nothing) = do
  now <- Time.now
  formatted <- Time.toText app.time.format now
  Out.Text.putStrLn app.out.info formatted
runCommands app (Now (Just format)) = do
  now <- Time.now
  formatted <- Time.toText (parseFormat format) now
  Out.Text.putStrLn app.out.info formatted
runCommands app (Elapse format timestamp interval) = do
  ts <- timeStampToUNIXOrNow timestamp
  let elapsed = Time.elapse (convertInterval interval) ts
  formatted <- Time.toText app.time.format elapsed
  Out.Text.putStrLn app.out.info formatted
runCommands app (Range format timestamp (Take n) interval) = do
  ts <- timeStampToUNIXOrNow timestamp
  let tsRange = Time.range ts n . convertInterval $ interval
  formattedTs <- mapM (Time.toText app.time.format) tsRange
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

parseFormat :: Format -> Time.Format
parseFormat UNIXMS = Time.UNIX Time.MS
parseFormat UNIXS = Time.UNIX Time.S
parseFormat ISO8601 = Time.ISO8601
parseFormat (Custom f) = Time.Custom f
