module CLI where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
import qualified UNIX

newtype Opts = Opts {optCommand :: Command}

data Command
  = Now (Maybe Format)
  | Elapse (Maybe Format) Offset
  | Range (Maybe Format) TimeStamp Take Step
  | Seconds Quantity

data Format = UNIX | ISO8601

data Offset = Offset
  { secondOffset :: Maybe Integer,
    minuteOffset :: Maybe Integer,
    hourOffset :: Maybe Integer,
    dayOffset :: Maybe Integer
  }

data Quantity = Quantity
  { secondQty :: Maybe Integer,
    minuteQty :: Maybe Integer,
    hourQty :: Maybe Integer,
    dayQty :: Maybe Integer
  }

newtype Take = Take Integer

newtype TimeStamp = TimeStamp Integer

newtype Step = Step Integer

run :: IO ()
run = do
  opts <- execParser optsParser
  case optCommand opts of
    Now format -> execNow format
    Elapse format offset -> execElapse format offset
    Range format ts t step -> execRange format ts t step
    Seconds qty -> execSeconds qty
  where
    commandOptions :: Parser Opts
    commandOptions =
      Opts
        <$> subparser
          ( nowCommand
              <> elapseCommand
              <> rangeCommand
              <> secondsCommand
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
            <*> offsetOptions

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
            <*> stepOptions

    secondsCommand :: Mod CommandFields Command
    secondsCommand =
      command
        "seconds"
        ( info
            (secondsOptions <**> helper)
            (progDesc "Prints the number of seconds")
        )
      where
        secondsOptions :: Parser Command
        secondsOptions = Seconds <$> quantityOptions

    execNow :: Maybe Format -> IO ()
    execNow format = displayTime =<< UNIX.now
      where
        displayTime = putStrLn . formatUNIXSeconds format

    execElapse :: Maybe Format -> Offset -> IO ()
    execElapse format offset = displayTime =<< getTime offset
      where
        getTime = UNIX.elapse . offsetToSeconds
        displayTime = putStrLn . formatUNIXSeconds format

    execRange :: Maybe Format -> TimeStamp -> Take -> Step -> IO ()
    execRange format (TimeStamp ts) (Take n) (Step s) = displayTime t
      where
        t = UNIX.range (realToFrac ts) n s
        displayTime = putStrLn . List.intercalate "\n" . map (formatUNIXSeconds format)

    execSeconds :: Quantity -> IO ()
    execSeconds = putStrLn . formatUNIXSeconds Nothing . quantityToSeconds

    formatUNIXSeconds :: Maybe Format -> UNIX.UNIXTime -> String
    formatUNIXSeconds Nothing = UNIX.toUNIXString
    formatUNIXSeconds (Just UNIX) = UNIX.toUNIXString
    formatUNIXSeconds (Just ISO8601) = UNIX.toISO8601String

    calculateUNIXTime :: [Integer] -> [Integer] -> UNIX.UNIXTime
    calculateUNIXTime weights coeffs = t
      where
        pairs = zip coeffs weights
        values = map (uncurry (*)) pairs
        t = realToFrac . sum $ values

    offsetToSeconds :: Offset -> UNIX.UNIXTime
    offsetToSeconds (Offset s m h d) = totalOffset
      where
        secondsWeight = 1
        minutesWeight = secondsWeight * 60
        hoursWeight = minutesWeight * 60
        daysWeight = hoursWeight * 24
        weights = [secondsWeight, minutesWeight, hoursWeight, daysWeight]
        coeffs = map (Maybe.fromMaybe 0) [s, m, h, d]
        totalOffset = calculateUNIXTime weights coeffs

    quantityToSeconds :: Quantity -> UNIX.UNIXTime
    quantityToSeconds (Quantity s m h d) = totalQty
      where
        secondsWeight = 1
        minutesWeight = secondsWeight * 60
        hoursWeight = minutesWeight * 60
        daysWeight = hoursWeight * 24
        weights = [secondsWeight, minutesWeight, hoursWeight, daysWeight]
        coeffs = map (Maybe.fromMaybe 0) [s, m, h, d]
        totalQty = calculateUNIXTime weights coeffs

formatOptions :: Parser (Maybe Format)
formatOptions = optional $ unixFormat <|> isoFormat
  where
    unixFormat :: Parser Format
    unixFormat =
      flag'
        UNIX
        (long "unix" <> help "unix format")

    isoFormat :: Parser Format
    isoFormat =
      flag'
        ISO8601
        ( long "iso8601"
            <> long "iso"
            <> help "ISO 8601 format"
        )

offsetOptions :: Parser Offset
offsetOptions =
  Offset
    <$> optional days
    <*> optional hours
    <*> optional minutes
    <*> optional seconds
  where
    seconds :: Parser Integer
    seconds =
      option
        auto
        ( long "second"
            <> long "sec"
            <> short 's'
            <> metavar "SECOND_OFFSET"
            <> help "number of seconds relative to the current time"
        )

    minutes :: Parser Integer
    minutes =
      option
        auto
        ( long "minute"
            <> long "min"
            <> short 'm'
            <> metavar "MINUTE_OFFSET"
            <> help "number of minutes relative to the current time"
        )

    hours :: Parser Integer
    hours =
      option
        auto
        ( long "hour"
            <> long "hr"
            <> short 'h'
            <> metavar "HOUR_OFFSET"
            <> help "number of hours relative to the current time"
        )

    days :: Parser Integer
    days =
      option
        auto
        ( long "day"
            <> long "dy"
            <> short 'd'
            <> metavar "DAY_OFFSET"
            <> help "number of days relative to the current time"
        )

quantityOptions :: Parser Quantity
quantityOptions =
  Quantity
    <$> optional seconds
    <*> optional minutes
    <*> optional hours
    <*> optional days
  where
    seconds :: Parser Integer
    seconds =
      option
        auto
        ( long "second"
            <> long "sec"
            <> short 's'
            <> metavar "SECOND_QTY"
            <> help "number of seconds"
        )

    hours :: Parser Integer
    hours =
      option
        auto
        ( long "hour"
            <> long "hr"
            <> short 'h'
            <> metavar "HOUR_QTY"
            <> help "number of hours"
        )

    minutes :: Parser Integer
    minutes =
      option
        auto
        ( long "minute"
            <> long "min"
            <> short 'm'
            <> metavar "MINUTE_QTY"
            <> help "number of minutes"
        )
    days :: Parser Integer
    days =
      option
        auto
        ( long "day"
            <> long "dy"
            <> short 'd'
            <> metavar "DAY_QTY"
            <> help "number of days"
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

stepOptions :: Parser Step
stepOptions =
  Step
    <$> option
      auto
      ( long "step"
          <> short 's'
          <> metavar "STEP"
          <> help "value to be stepped over in seconds"
      )
