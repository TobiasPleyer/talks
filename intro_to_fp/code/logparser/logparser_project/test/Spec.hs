{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import Test.Hspec
import Test.Hspec.Attoparsec
import Control.Exception (evaluate)
import Test.QuickCheck

import Lib
import PropertyBased


ok_ip    = "127.0.0.1"  :: B.ByteString
ok_day   = "2013-06-29" :: B.ByteString
ok_time  = "12:52:17"   :: B.ByteString
ok_prod  = "monitor"    :: B.ByteString
ok_entry = B.intercalate " " [ok_day,ok_time,ok_ip,ok_prod]

nok_ip    = "127.0.1"    :: B.ByteString
nok_day   = "2013/06/29" :: B.ByteString
nok_time  = "12:52"      :: B.ByteString
nok_prod  = "foobar"     :: B.ByteString
nok_entry = B.intercalate " " [nok_day,nok_time,nok_ip,nok_prod]


main :: IO ()
main = do
  hspec spec


spec :: Spec
spec = do
  describe "TDD test suite for our log parser" $ do
    describe "Positive Tests" $ do
      it "Successfully parse an IP address" $
        parseIP `shouldSucceedOn` ok_ip
      it "Successfully parse a date" $
        parseDay `shouldSucceedOn` ok_day
      it "Successfully parse a time" $
        parseTime `shouldSucceedOn` ok_time
      it "Successfully parse a product" $
        parseProduct `shouldSucceedOn` ok_prod
      it "Successfully parse a log entry" $
        parseLogEntry `shouldSucceedOn` ok_entry
    describe "Negative Tests" $ do
      it "Fail on too short IP address" $
        parseIP `shouldFailOn` nok_ip
      it "Fail on wrong data separator" $
        parseDay `shouldFailOn` nok_day
      it "Fail on too short time" $
        parseTime `shouldFailOn` nok_time
      it "Fail on unknown product" $
        parseProduct `shouldFailOn` nok_prod
      it "Fail on invalid log entry" $
        parseLogEntry `shouldFailOn` nok_entry
