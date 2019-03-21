module PropertyBased where

import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BC
import Lib
import Test.QuickCheck

newtype MyInt = MyInt Int deriving (Show, Eq, Ord)

instance Arbitrary MyInt where
  arbitrary = MyInt `fmap` choose (0,1000)

ip_property :: (MyInt,MyInt,MyInt,MyInt) -> Bool
ip_property (MyInt n1, MyInt n2, MyInt n3, MyInt n4) =
  let
    testString = BC.pack $ show n1 ++ "." ++ show n2 ++ "." ++ show n3 ++ "." ++ show n4
    parse_result = parseOnly parseIP testString
  in
    case parse_result of
      Left err -> False
      Right (IP w1 w2 w3 w4) -> let
                                  n1' = fromIntegral w1
                                  n2' = fromIntegral w2
                                  n3' = fromIntegral w3
                                  n4' = fromIntegral w4
                                in (n1==n1')&&(n2==n2')&&(n3==n3')&&(n4==n4')
