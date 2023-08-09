{-# LANGUAGE OverloadedRecordDot #-}

module EnvSpec where

import qualified Data.Maybe as Maybe
import qualified Env
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC

instance QC.Arbitrary Env.Config' where
  arbitrary =
    Env.Config'
      <$> (QC.arbitrary :: QC.Gen (Maybe Env.Format))
      <*> (QC.arbitrary :: QC.Gen (Maybe Env.UNIXConfig'))

instance QC.Arbitrary Env.Format where
  arbitrary = QC.oneof . map return $ [Env.UNIX, Env.ISO8601]

instance QC.Arbitrary Env.UNIXConfig' where
  arbitrary = Env.UNIXConfig' <$> (QC.arbitrary :: QC.Gen (Maybe Env.UNIXPrecision))

instance QC.Arbitrary Env.UNIXPrecision where
  arbitrary = QC.oneof . map return $ [Env.MS, Env.S]

spec :: H.Spec
spec = do
  fromConfig'Spec

fromConfig'Spec :: H.Spec
fromConfig'Spec = H.describe "fromConfig'" $ do
  H.it "should use default values for missing values" $ do
    QC.property $ \config' -> do
      let config = Env.fromConfig' config'
      case config'.defaultFormat' of
        Nothing -> config.defaultFormat `H.shouldBe` Env.defaultConfig.defaultFormat
        Just format -> config.defaultFormat `H.shouldBe` format
      case config'.unixConfig' of
        Nothing -> config.unixConfig `H.shouldBe` Env.defaultConfig.unixConfig
        Just unix -> do
          case unix.precision' of
            Nothing -> config.unixConfig.precision `H.shouldBe` Env.defaultConfig.unixConfig.precision
            Just p -> config.unixConfig.precision `H.shouldBe` p
