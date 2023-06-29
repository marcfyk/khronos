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
import qualified Unix

newtype Opts = Opts {optCommand :: Command}

data Command
  = Now (Maybe Format)
  | Elapse (Maybe Format) ElapseOffset

data Format = Unix | ISO8601

data ElapseOffset = ElapseOffset
  { days :: Maybe Integer,
    hours :: Maybe Integer,
    minutes :: Maybe Integer,
    seconds :: Maybe Integer
  }

run :: IO ()
run = do
  opts <- execParser optsParser
  case optCommand opts of
    Now format -> execNow format
    Elapse format offset -> execElapse format offset
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
      where
        formatOptions :: Parser (Maybe Format)
        formatOptions = optional $ unixFormat <|> isoFormat
          where
            unixFormat :: Parser Format
            unixFormat =
              flag'
                Unix
                (long "unix" <> help "unix format")

            isoFormat :: Parser Format
            isoFormat =
              flag'
                ISO8601
                ( long "iso8601"
                    <> long "iso"
                    <> help "ISO 8601 format"
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

            offsetOptions :: Parser ElapseOffset
            offsetOptions =
              ElapseOffset
                <$> dayOffset
                <*> hourOffset
                <*> minuteOffset
                <*> secondOffset

            dayOffset :: Parser (Maybe Integer)
            dayOffset =
              optional . option auto $
                ( long "day"
                    <> long "dy"
                    <> short 'd'
                    <> metavar "DAY_OFFSET"
                    <> help "number of days relative to the current time"
                )

            hourOffset :: Parser (Maybe Integer)
            hourOffset =
              optional . option auto $
                ( long "hour"
                    <> long "hr"
                    <> short 'h'
                    <> metavar "HOUR_OFFSET"
                    <> help "number of hours relative to the current time"
                )
            minuteOffset :: Parser (Maybe Integer)
            minuteOffset =
              optional . option auto $
                ( long "minute"
                    <> long "min"
                    <> short 'm'
                    <> metavar "MINUTE_OFFSET"
                    <> help "number of minutes relative to the current time"
                )

            secondOffset :: Parser (Maybe Integer)
            secondOffset =
              optional . option auto $
                ( long "second"
                    <> long "sec"
                    <> short 's'
                    <> metavar "SECOND_OFFSET"
                    <> help "number of seconds relative to the current time"
                )

    formatUnixSeconds :: Maybe Format -> Unix.Unix -> String
    formatUnixSeconds Nothing = Unix.toUnixString
    formatUnixSeconds (Just Unix) = Unix.toUnixString
    formatUnixSeconds (Just ISO8601) = Unix.toISO8601String

    execNow :: Maybe Format -> IO ()
    execNow format = Unix.now >>= putStrLn . formatUnixSeconds format

    execElapse :: Maybe Format -> ElapseOffset -> IO ()
    execElapse format offset = Unix.elapse totalOffset >>= putStrLn . formatUnixSeconds format
      where
        secondsWeight = 1
        minutesWeight = secondsWeight * 60
        hoursWeight = minutesWeight * 60
        daysWeight = hoursWeight * 24

        offsetCoeffs = map (\f -> Maybe.fromMaybe 0 . f $ offset) [seconds, minutes, hours, days]
        offsetPairs = zip offsetCoeffs [secondsWeight, minutesWeight, hoursWeight, daysWeight]
        offsetValues = map (uncurry (*)) offsetPairs
        totalOffset = realToFrac . sum $ offsetValues
