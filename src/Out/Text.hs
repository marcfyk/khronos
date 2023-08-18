module Out.Text where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Out
import qualified System.Console.ANSI as ANSI

putStrLn :: Out.Out -> Out.MessageStyle -> T.Text -> IO ()
putStrLn out messageStyle text = do
  let underlineSGR =
        ANSI.SetUnderlining $
          if Out.isUnderlined messageStyle
            then ANSI.SingleUnderline
            else ANSI.NoUnderline
  let boldSGR =
        ANSI.SetConsoleIntensity $
          if Out.isBold messageStyle
            then ANSI.BoldIntensity
            else ANSI.NormalIntensity
  let sgrs = Out.colorFG messageStyle <> Out.colorBG messageStyle <> [underlineSGR, boldSGR]
  ANSI.setSGR sgrs
  TIO.putStrLn text
  ANSI.setSGR [ANSI.Reset]
