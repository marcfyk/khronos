{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Control.Monad as Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Text.Printf as Printf

data Config = Config
  { format :: Maybe Format,
    unixConfig :: Maybe UNIXConfig
  }
  deriving (Generics.Generic, Eq)

defaultConfig :: Config
defaultConfig =
  Config
    { format = Just defaultFormat,
      unixConfig = Just defaultUNIXConfig
    }

instance A.FromJSON Config where
  parseJSON = A.withObject "Config" $ \v -> Config <$> v A..:? "format" <*> v A..:? "unix"

instance A.ToJSON Config where
  toJSON (Config format' unix') = A.object ["format" A..= format', "unix" A..= unix']
  toEncoding (Config format' unix') = A.pairs ("format" A..= format' <> "unix" A..= unix')

data Format
  = UNIX
  | ISO8601
  | Custom String
  deriving (Generics.Generic, Eq)

instance Show Format where
  show UNIX = "UNIX"
  show ISO8601 = "ISO8601"
  show (Custom f) = Printf.printf "custom format (%v)" f

defaultFormat :: Format
defaultFormat = UNIX

instance A.FromJSON Format where
  parseJSON (A.String "unix") = return UNIX
  parseJSON (A.String "iso8601") = return ISO8601
  parseJSON (A.String s) = return . Custom . T.unpack $ s
  parseJSON _ = fail "format should be \"unix\" | \"iso8601\""

instance A.ToJSON Format where
  toJSON UNIX = A.String "unix"
  toJSON ISO8601 = A.String "iso8601"
  toJSON (Custom s) = A.String . T.pack $ s

newtype UNIXConfig = UNIXConfig
  { precision :: Maybe UNIXPrecision
  }
  deriving (Generics.Generic, Eq)

defaultUNIXConfig :: UNIXConfig
defaultUNIXConfig = UNIXConfig (Just defaultUNIXPrecision)

instance A.FromJSON UNIXConfig where
  parseJSON = A.withObject "UNIXConfig" $ \v -> UNIXConfig <$> v A..: "precision"

instance A.ToJSON UNIXConfig where
  toJSON (UNIXConfig precision) = A.object ["precision" A..= precision]
  toEncoding (UNIXConfig precision) = A.pairs ("precision" A..= precision)

data UNIXPrecision
  = MS
  | S
  deriving (Generics.Generic, Eq)

instance Show UNIXPrecision where
  show MS = "milliseconds (ms)"
  show S = "seconds (s)"

defaultUNIXPrecision :: UNIXPrecision
defaultUNIXPrecision = MS

instance A.FromJSON UNIXPrecision where
  parseJSON (A.String "ms") = return MS
  parseJSON (A.String "s") = return S
  parseJSON _ = fail "unix_precision should be \"ms\" | \"s\""

instance A.ToJSON UNIXPrecision where
  toJSON MS = A.String "ms"
  toJSON S = A.String "s"

data NoConfig
  = NoFile
  | InvalidFile FilePath String
  deriving (Show, Eq)

paths :: [String]
paths =
  [ ".khronos.json",
    "/.config/khronos/khronos.json"
  ]

getPaths :: IO [FilePath]
getPaths = do
  homeDir <- Dir.getHomeDirectory
  return . map (FP.joinPath . (homeDir :) . return) $ paths

tryConfigFromFile :: IO (Either NoConfig (FilePath, Config))
tryConfigFromFile = do
  filePaths <- getPaths
  existingFilePaths <- Monad.filterM Dir.doesFileExist filePaths
  case existingFilePaths of
    [] -> return . Left $ NoFile
    (filePath : _) -> do
      decoded <- fmap A.eitherDecode . BS.readFile $ filePath
      return $ case decoded of
        Left err -> Left . InvalidFile filePath $ err
        Right config -> Right (filePath, config)

load :: IO (Either Config.NoConfig FilePath, Config.Config)
load = do
  config <- tryConfigFromFile
  return $ case config of
    Left err -> (Left err, Config.defaultConfig)
    Right (filePath, config) -> (Right filePath, config)
