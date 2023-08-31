{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI (run) where

import qualified Config
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Time
import qualified Options.Applicative as OA
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

data Format
  = UNIXS
  | UNIXMS
  | ISO8601
  | Custom String (Maybe Int)

type Milliseconds = Integer

type Seconds = Integer

type Minutes = Integer

type Hours = Integer

type Days = Integer

data Interval
  = Interval
      (Maybe Milliseconds)
      (Maybe Seconds)
      (Maybe Minutes)
      (Maybe Hours)
      (Maybe Days)

newtype Take = Take Integer

data TimeStamp
  = TSUNIX Integer
  | TSNow

run :: IO ()
run = do
  c <- Config.load
  app <- newApp c
  opts <- runParser
  let command = optCommand opts
  runCommands app command

newApp :: Either Config.NoConfig (FilePath, Config.Config) -> IO App
newApp c = do
  t <- Time.newTime c
  let o = Out.newOut
  return $ App c t o

runParser :: IO Opts
runParser = OA.execParser optsParser
  where
    commandOptions :: OA.Parser Opts
    commandOptions =
      Opts
        <$> OA.subparser
          ( envCommand
              <> nowCommand
              <> elapseCommand
              <> rangeCommand
          )

    optsParser :: OA.ParserInfo Opts
    optsParser =
      OA.info
        (commandOptions OA.<**> OA.helper)
        ( OA.fullDesc
            <> OA.progDesc "khronos provides commands to display the result of time operations"
            <> OA.header "khronos - A CLI for time"
        )

    envCommand :: OA.Mod OA.CommandFields Command
    envCommand =
      OA.command
        "env"
        ( OA.info
            (envOptions OA.<**> OA.helper)
            (OA.progDesc "Prints the current loaded configuration")
        )
      where
        envOptions :: OA.Parser Command
        envOptions = pure Env

    nowCommand :: OA.Mod OA.CommandFields Command
    nowCommand =
      OA.command
        "now"
        ( OA.info
            (nowOptions OA.<**> OA.helper)
            (OA.progDesc "Prints the current time in the specified format (default format is unix)")
        )
      where
        nowOptions :: OA.Parser Command
        nowOptions = Now <$> formatOptions

    elapseCommand :: OA.Mod OA.CommandFields Command
    elapseCommand =
      OA.command
        "elapse"
        ( OA.info
            (elapseOptions OA.<**> OA.helper)
            (OA.progDesc "Prints the elapse time from a given offset in the specified format (default format is unix)")
        )
      where
        elapseOptions :: OA.Parser Command
        elapseOptions =
          Elapse
            <$> formatOptions
            <*> timestampOptions
            <*> intervalOptions

    rangeCommand :: OA.Mod OA.CommandFields Command
    rangeCommand =
      OA.command
        "range"
        ( OA.info
            (rangeOptions OA.<**> OA.helper)
            (OA.progDesc "Prints a range of time starting from a timestamp (unix seconds) and stepped over with a specific value")
        )
      where
        rangeOptions :: OA.Parser Command
        rangeOptions =
          Range
            <$> formatOptions
            <*> timestampOptions
            <*> takeOptions
            <*> intervalOptions

    formatOptions :: OA.Parser (Maybe Format)
    formatOptions =
      OA.optional $
        unixSParser
          OA.<|> unixMSParser
          OA.<|> isoParser
          OA.<|> customParser
      where
        unixSParser :: OA.Parser Format
        unixSParser = OA.flag' UNIXS (OA.long "unix-s" <> OA.help "unix format in seconds")

        unixMSParser :: OA.Parser Format
        unixMSParser = OA.flag' UNIXMS (OA.long "unix-ms" <> OA.help "unix format in milliseconds")

        isoParser :: OA.Parser Format
        isoParser =
          OA.flag'
            ISO8601
            ( OA.long "iso8601"
                <> OA.long "iso"
                <> OA.help "ISO8601 format"
            )

        customParser :: OA.Parser Format
        customParser =
          Custom
            <$> formatParser
            <*> utcOffsetParser
          where
            formatParser =
              OA.strOption
                ( OA.long "format"
                    <> OA.short 'f'
                    <> OA.metavar "FORMAT"
                    <> OA.help "format for representing time, example: %Y-%m-%d %H:%M:%S"
                )
            utcOffsetParser =
              OA.optional $
                OA.option
                  OA.auto
                  ( OA.long "utc"
                      <> OA.metavar "UTC_OFFSET"
                      <> OA.help "utc offset to the timestamp"
                  )

    intervalOptions :: OA.Parser Interval
    intervalOptions =
      Interval
        <$> OA.optional millisecondParser
        <*> OA.optional secondParser
        <*> OA.optional minuteParser
        <*> OA.optional hourParser
        <*> OA.optional dayParser
      where
        millisecondParser :: OA.Parser Milliseconds
        millisecondParser =
          OA.option
            OA.auto
            ( OA.long "milliseconds"
                <> OA.long "ms"
                <> OA.metavar "MILLISECOND_OFFSET"
                <> OA.help "number of milliseconds relative to the timestamp"
            )

        secondParser :: OA.Parser Seconds
        secondParser =
          OA.option
            OA.auto
            ( OA.long "second"
                <> OA.long "sec"
                <> OA.short 'S'
                <> OA.metavar "SECOND_OFFSET"
                <> OA.help "number of seconds relative to the timestamp"
            )

        minuteParser :: OA.Parser Minutes
        minuteParser =
          OA.option
            OA.auto
            ( OA.long "minute"
                <> OA.long "min"
                <> OA.short 'M'
                <> OA.metavar "MINUTE_OFFSET"
                <> OA.help "number of minutes relative to the timestamp"
            )

        hourParser :: OA.Parser Hours
        hourParser =
          OA.option
            OA.auto
            ( OA.long "hour"
                <> OA.long "hr"
                <> OA.short 'H'
                <> OA.metavar "HOUR_OFFSET"
                <> OA.help "number of hours relative to the timestamp"
            )

        dayParser :: OA.Parser Days
        dayParser =
          OA.option
            OA.auto
            ( OA.long "day"
                <> OA.short 'd'
                <> OA.metavar "DAY_OFFSET"
                <> OA.help "number of days relative to the timestamp"
            )

    takeOptions :: OA.Parser Take
    takeOptions =
      Take
        <$> OA.option
          OA.auto
          ( OA.long "take"
              <> OA.short 't'
              <> OA.metavar "TAKE_AMOUNT"
              <> OA.help "number of values to take"
          )

    timestampOptions :: OA.Parser (Maybe TimeStamp)
    timestampOptions = OA.optional $ unixParser OA.<|> nowParser
      where
        unixParser =
          TSUNIX
            <$> OA.option
              OA.auto
              ( OA.long "timestamp"
                  <> OA.long "ts"
                  <> OA.metavar "TIMESTAMP"
                  <> OA.help "timestamp"
              )
        nowParser = OA.flag' TSNow (OA.long "now" <> OA.help "current timestamp")

runCommands :: App -> Command -> IO ()
runCommands app Env = runEnvCommand app
runCommands app (Now format) = runNowCommand app format
runCommands app (Elapse format timestamp interval) =
  runElapseCommand app format timestamp interval
runCommands app (Range format timestamp takeN interval) =
  runRangeCommand app format timestamp takeN interval

runEnvCommand :: App -> IO ()
runEnvCommand app = do
  let filePathResult = fst <$> app.configResult
  let configResult' = snd <$> app.configResult
  let unixConfig = Config.unixConfig <$> configResult'
  loadedConfigMessage filePathResult
  generalConfigHeader filePathResult
  generalConfigMessage configResult'
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
    generalConfigMessage config = do
      formatMessage (Config.format <$> config)
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
    unixConfigHeader (Right (Just _)) =
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

runNowCommand :: App -> Maybe Format -> IO ()
runNowCommand app format = do
  now <- Time.now
  let format' = parseFormat format app.time.timezone
  let formatted = Time.toText format' now
  Out.Text.putStrLn app.out.info formatted

runElapseCommand :: App -> Maybe Format -> Maybe TimeStamp -> Interval -> IO ()
runElapseCommand app format timestamp interval = do
  ts <- timeStampToUNIXOrNow timestamp
  let elapsed = Time.elapse (convertInterval interval) ts
  let format' = parseFormat format app.time.timezone
  let formatted = Time.toText format' elapsed
  Out.Text.putStrLn app.out.info formatted

runRangeCommand :: App -> Maybe Format -> Maybe TimeStamp -> Take -> Interval -> IO ()
runRangeCommand app format timestamp (Take n) interval = do
  ts <- timeStampToUNIXOrNow timestamp
  let tsRange = Time.range ts n . convertInterval $ interval
  let format' = parseFormat format app.time.timezone
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
parseFormat Nothing _ = Time.UNIX Time.S
