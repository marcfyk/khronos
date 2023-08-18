module Out.Text where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Out
import qualified System.Console.ANSI as ANSI

putStr :: Out.MessageStyle -> T.Text -> IO ()
putStr messageStyle text = do
  Out.setStyling messageStyle
  TIO.putStr text
  ANSI.setSGR [ANSI.Reset]

putStrLn :: Out.MessageStyle -> T.Text -> IO ()
putStrLn messageStyle text = do
  Out.setStyling messageStyle
  TIO.putStrLn text
  ANSI.setSGR [ANSI.Reset]
