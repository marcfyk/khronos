module CLI where

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
  | Range (Maybe Format) Take TimeStamp

data Format = UNIX | ISO8601

data Offset = Offset
  { days :: Maybe Integer,
    hours :: Maybe Integer,
    minutes :: Maybe Integer,
    seconds :: Maybe Integer
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
    Range format take step -> undefined
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (commandOptions <**> helper)
        ( fullDesc
            <> progDesc "khronos provides commands to display the result of time operations"
            <> header "khronos - A CLI for time"
        )

    commandOptions :: Parser Opts
    commandOptions =
      Opts
        <$> subparser
          ( nowCommand
              <> elapseCommand
          )
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
        <$> optional dayOffset
        <*> optional hourOffset
        <*> optional minuteOffset
        <*> optional secondOffset
      where
        dayOffset :: Parser Integer
        dayOffset =
          option
            auto
            ( long "day"
                <> long "dy"
                <> short 'd'
                <> metavar "DAY_OFFSET"
                <> help "number of days relative to the current time"
            )

        hourOffset :: Parser Integer
        hourOffset =
          option
            auto
            ( long "hour"
                <> long "hr"
                <> short 'h'
                <> metavar "HOUR_OFFSET"
                <> help "number of hours relative to the current time"
            )
        minuteOffset :: Parser Integer
        minuteOffset =
          option
            auto
            ( long "minute"
                <> long "min"
                <> short 'm'
                <> metavar "MINUTE_OFFSET"
                <> help "number of minutes relative to the current time"
            )

        secondOffset :: Parser Integer
        secondOffset =
          option
            auto
            ( long "second"
                <> long "sec"
                <> short 's'
                <> metavar "SECOND_OFFSET"
                <> help "number of seconds relative to the current time"
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
        elapseOptions = Elapse <$> formatOptions <*> offsetOptions

    formatUnixSeconds :: Maybe Format -> UNIX.UNIXTime -> String
    formatUnixSeconds Nothing = UNIX.toUNIXString
    formatUnixSeconds (Just UNIX) = UNIX.toUNIXString
    formatUnixSeconds (Just ISO8601) = UNIX.toISO8601String

    offsetToSeconds :: Offset -> UNIX.UNIXTime
    offsetToSeconds offset = totalOffset
      where
        secondsWeight = 1
        minutesWeight = secondsWeight * 60
        hoursWeight = minutesWeight * 60
        daysWeight = hoursWeight * 24

        offsetCoeffs = map (\f -> Maybe.fromMaybe 0 . f $ offset) [seconds, minutes, hours, days]
        offsetPairs = zip offsetCoeffs [secondsWeight, minutesWeight, hoursWeight, daysWeight]
        offsetValues = map (uncurry (*)) offsetPairs
        totalOffset = realToFrac . sum $ offsetValues

    execNow :: Maybe Format -> IO ()
    execNow format = UNIX.now >>= putStrLn . formatUnixSeconds format

    execElapse :: Maybe Format -> Offset -> IO ()
    execElapse format offset = result >>= putStrLn . formatUnixSeconds format
      where
        result = UNIX.elapse . offsetToSeconds $ offset
