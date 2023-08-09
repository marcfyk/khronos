{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TimeSpec (spec) where

import qualified Data.Fixed
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as HQC
import qualified Test.QuickCheck as QC
import qualified Time

instance QC.Arbitrary Time.UnixTime where
  arbitrary = (realToFrac :: Double -> Time.UnixTime) <$> (QC.arbitrary :: QC.Gen Double)

instance QC.Arbitrary Time.UNIXPrecision where
  arbitrary = QC.oneof . map return $ [Time.MS, Time.S]

instance QC.Arbitrary Time.Format where
  arbitrary = QC.oneof [Time.UNIX <$> unixPrecision, return Time.ISO8601]
    where
      unixPrecision = QC.arbitrary :: QC.Gen Time.UNIXPrecision

instance QC.Arbitrary Time.Unit where
  arbitrary = QC.oneof . map return $ [Time.Second, Time.Minute, Time.Hour, Time.Day]

instance QC.Arbitrary Time.Interval where
  arbitrary = Time.Interval <$> unit <*> coeff
    where
      unit = QC.arbitrary :: QC.Gen Time.Unit
      coeff = QC.arbitrary :: QC.Gen Integer

spec :: H.Spec
spec = do
  toTextSpec
  unixCoeffSpec
  intervalCoeffSpec
  elapseSpec

toTextSpec :: H.Spec
toTextSpec = H.describe "toText" $ do
  H.describe "it should display the time in the correct format" $ do
    H.describe "when format: UNIX MS" $ do
      HQC.prop "should return the whole number of x shifted by 10^3" $ \x ->
        Time.toText (Time.UNIX Time.MS) x `H.shouldBe` (toWholeNumberAsText . (* 1000) $ x)
    H.describe "when format: UNIX S" $ do
      HQC.prop "should return the whole number of x" $ \x ->
        Time.toText (Time.UNIX Time.S) x `H.shouldBe` toWholeNumberAsText x
    H.describe "when  format: ISO8601" $ do
      HQC.prop "should return the default ISO8601 representation of x" $ \x ->
        Time.toText Time.ISO8601 x `H.shouldBe` toDefaultISO x
  where
    toWholeNumberAsText :: Time.UnixTime -> T.Text
    toWholeNumberAsText = T.pack . show . (round :: Time.UnixTime -> Integer)
    toDefaultISO :: Time.UnixTime -> T.Text
    toDefaultISO = T.pack . ISO8601.iso8601Show . POSIX.posixSecondsToUTCTime

unixCoeffSpec :: H.Spec
unixCoeffSpec = H.describe "unixCoeff" $ do
  H.describe "it should return representation in seconds" $ do
    H.describe "when unit: Milisecond" $ do
      H.it "should return as 10^-3" $ do
        Time.unixCoeff Time.Millisecond `H.shouldBe` 0.001
    H.describe "when unit: Second" $ do
      H.it "should return as 1" $ do
        Time.unixCoeff Time.Second `H.shouldBe` 1
    H.describe "when unit: Minute" $ do
      H.it "should return as 60" $ do
        Time.unixCoeff Time.Minute `H.shouldBe` 60
    H.describe "when unit: Hour" $ do
      H.it "should return as 3600" $ do
        Time.unixCoeff Time.Hour `H.shouldBe` 3600
    H.describe "when unit: Day" $ do
      H.it "should return as 86400" $ do
        Time.unixCoeff Time.Day `H.shouldBe` 86400

intervalCoeffSpec :: H.Spec
intervalCoeffSpec = H.describe "intervalCoeff" $ do
  HQC.prop "any coeff of 0 will return 0" $ \unit ->
    Time.intervalToUnixCoeff (Time.Interval unit 0) `H.shouldBe` 0
  HQC.prop "any coeff of 1 will return unixCoeff unit" $ \unit ->
    Time.intervalToUnixCoeff (Time.Interval unit 1) `H.shouldBe` Time.unixCoeff unit
  H.describe "given an interval" $ do
    HQC.prop "it should return the representation of the interval in seconds" $ \interval@(Time.Interval unit coeff) ->
      Time.intervalToUnixCoeff interval `H.shouldBe` Time.unixCoeff unit * realToFrac coeff

elapseSpec :: H.Spec
elapseSpec = H.describe "elapse" $ do
  H.describe "given an empty list" $ do
    HQC.prop "should always return the same timestamp" $ \ts ->
      Time.elapse [] ts `H.shouldBe` ts
  H.describe "given a list of intervals and an unix timestamp" $ do
    H.it "it should return a unix timestamp shifted by the interval" $ do
      QC.property $ \intervals ->
        QC.property $ \ts ->
          let totalInterval = (sum . map Time.intervalToUnixCoeff $ intervals)
           in Time.elapse intervals ts `H.shouldBe` ts + totalInterval

rangeSpec :: H.Spec
rangeSpec = H.describe "range" $ do
  H.describe "given an 0 n" $ do
    H.it "should always return empty list" $ do
      QC.property $ \ts ->
        QC.property $ \intervals ->
          Time.range ts 0 intervals `H.shouldBe` []
  H.describe "given empty intervals" $ do
    H.it "should return ts in a list of size n" $ do
      QC.property $ \ts ->
        QC.property $ \n ->
          Time.range ts n [] `H.shouldBe` replicate (fromIntegral n) ts
  H.it "it should always return a list of ts incrementing with a step = intervals" $ do
    QC.property $ \ts ->
      QC.property $ \n ->
        QC.property $ \intervals ->
          let totalInterval = (sum . map Time.intervalToUnixCoeff $ intervals)
           in Time.range ts n intervals `H.shouldBe` [ts, ts + totalInterval]
