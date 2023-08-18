module Out where

import qualified System.Console.ANSI as ANSI

data Out = Out
  { info :: MessageStyle,
    warn :: MessageStyle,
    error :: MessageStyle
  }

data MessageStyle = MessageStyle
  { colorFG :: [ANSI.SGR],
    colorBG :: [ANSI.SGR],
    isUnderlined :: Bool,
    isBold :: Bool
  }

newOut :: Out
newOut = Out info warn error
  where
    info =
      MessageStyle
        { colorFG = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue],
          colorBG = [],
          isUnderlined = False,
          isBold = True
        }
    warn =
      MessageStyle
        { colorFG = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow],
          colorBG = [],
          isUnderlined = False,
          isBold = False
        }
    error =
      MessageStyle
        { colorFG = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red],
          colorBG = [],
          isUnderlined = False,
          isBold = False
        }

putStrLn :: Out -> MessageStyle -> String -> IO ()
putStrLn out messageStyle text = do
  let underlineSGR =
        ANSI.SetUnderlining $
          if isUnderlined messageStyle
            then ANSI.SingleUnderline
            else ANSI.NoUnderline
  let boldSGR =
        ANSI.SetConsoleIntensity $
          if isBold messageStyle
            then ANSI.BoldIntensity
            else ANSI.NormalIntensity
  let sgrs = colorFG messageStyle <> colorBG messageStyle <> [underlineSGR, boldSGR]
  ANSI.setSGR sgrs
  Prelude.putStrLn text
  ANSI.setSGR [ANSI.Reset]
