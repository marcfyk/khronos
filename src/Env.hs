{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import Control.Monad (filterM, (<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BS
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified GHC.Generics as Generics
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Text.Printf as Printf

data Env = Env
  { configPath :: Either NoConfig FilePath,
    config :: Config
  }
  deriving (Show)

getEnv :: IO Env
getEnv = do
  (configPath, config) <- getConfig
  return $ Env configPath config

displayEnv :: Env -> T.Text
displayEnv (Env configPath config) = totalTxt
  where
    configPathTxt :: TLB.Builder
    configPathTxt = case configPath of
      Left err -> TLB.fromString . show $ err
      Right fp -> TLB.fromString $ "using config file at: " <> fp

    configTxt :: TLB.Builder
    configTxt = ("config:\n" <>) . AP.encodePrettyToTextBuilder $ config

    totalTxt = TL.toStrict . TLB.toLazyText $ foldr (\acc t -> acc <> "\n" <> t) "" [configPathTxt, configTxt]

data Config = Config
  { defaultFormat :: Format,
    unixConfig :: UNIXConfig
  }
  deriving (Generics.Generic, Show)

instance A.FromJSON Config where
  parseJSON = A.withObject "Config" $ \v ->
    Config
      <$> v
      A..: "default_format"
      <*> v
      A..: "unix"

instance A.ToJSON Config where
  toJSON (Config format unix) = A.object ["default_format" A..= format, "unix" A..= unix]
  toEncoding (Config format unix) = A.pairs ("default_format" A..= format <> "unix" A..= unix)

data Config' = Config'
  { defaultFormat' :: Maybe Format,
    unixConfig' :: Maybe UNIXConfig'
  }
  deriving (Generics.Generic, Show)

instance A.FromJSON Config' where
  parseJSON = A.withObject "Config'" $ \v -> Config' <$> v A..:? "default_format" <*> v A..:? "unix"

instance A.ToJSON Config' where
  toJSON (Config' format' unix') = A.object ["default_format" A..= format', "unix" A..= unix']
  toEncoding (Config' format' unix') = A.pairs ("default_format" A..= format' <> "unix" A..= unix')

data Format = UNIX | ISO8601 deriving (Generics.Generic, Show)

instance A.FromJSON Format where
  parseJSON (A.String "unix") = return UNIX
  parseJSON (A.String "iso8601") = return ISO8601
  parseJSON _ = fail "format should be \"unix\" | \"iso8601\""

instance A.ToJSON Format where
  toJSON UNIX = A.String "unix"
  toJSON ISO8601 = A.String "iso8601"

newtype UNIXConfig = UNIXConfig
  { precision :: UNIXPrecision
  }
  deriving (Generics.Generic, Show)

instance A.FromJSON UNIXConfig where
  parseJSON = A.withObject "UNIXConfig" $ \v -> UNIXConfig <$> v A..: "precision"

instance A.ToJSON UNIXConfig where
  toJSON (UNIXConfig precision) = A.object ["precision" A..= precision]
  toEncoding (UNIXConfig precision) = A.pairs ("precision" A..= precision)

newtype UNIXConfig' = UNIXConfig'
  { precision' :: Maybe UNIXPrecision
  }
  deriving (Generics.Generic, Show)

instance A.FromJSON UNIXConfig' where
  parseJSON = A.withObject "UNIXConfig'" $ \v -> UNIXConfig' <$> v A..: "precision"

instance A.ToJSON UNIXConfig' where
  toJSON (UNIXConfig' precision') = A.object ["precision" A..= precision']
  toEncoding (UNIXConfig' precision') = A.pairs ("precision" A..= precision')

data UNIXPrecision = MS | S deriving (Generics.Generic, Show)

instance A.FromJSON UNIXPrecision where
  parseJSON (A.String "ms") = return MS
  parseJSON (A.String "s") = return S
  parseJSON _ = fail "unix_precision' should be \"ms\" | \"s\""

instance A.ToJSON UNIXPrecision where
  toJSON MS = A.String "ms"
  toJSON S = A.String "s"

defaultConfig :: Config
defaultConfig =
  Config
    { defaultFormat = UNIX,
      unixConfig = UNIXConfig MS
    }

paths :: [String]
paths =
  [ ".khronos.json",
    "/.config/khronos/khronos.json"
  ]

getPaths :: IO [FilePath]
getPaths = do
  homeDir <- Dir.getHomeDirectory
  return . map (FP.joinPath . (homeDir :) . return) $ paths

data NoConfig
  = NoFile
  | InvalidFile FilePath String

instance Show NoConfig where
  show NoFile = "no config file found"
  show (InvalidFile fp err) = Printf.printf "could not parse config from %s\n%s" fp err

getConfig' :: IO (Either NoConfig (FilePath, Config'))
getConfig' = do
  fps <- getPaths
  validFps <- filterM Dir.doesFileExist fps
  case validFps of
    [] -> return . Left $ NoFile
    (fp : _) -> do
      decoded <- fmap A.eitherDecode . BS.readFile $ fp
      case decoded of
        Left err -> do
          return . Left . InvalidFile fp $ err
        Right conf -> return . Right $ (fp, conf)

fromConfig' :: Config' -> Config
fromConfig' c' = c
  where
    c = Config format unix
    format = Maybe.fromMaybe UNIX c'.defaultFormat'
    unix = UNIXConfig p
      where
        p = Maybe.fromMaybe MS (precision' =<< c'.unixConfig')

getConfig :: IO (Either NoConfig FilePath, Config)
getConfig = do
  conf' <- getConfig'
  return $ case conf' of
    Left err -> (Left err, defaultConfig)
    Right (fp, conf') -> (Right fp, fromConfig' conf')
