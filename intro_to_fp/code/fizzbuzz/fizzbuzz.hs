#!/usr/bin/env stack
{- stack script --resolver lts-13.8 --package base -}

import Data.Foldable (traverse_)

showValue :: Int -> String
showValue i
    | i `mod` 15 == 0 = "FizzBuzz"
    | i `mod`  3 == 0 = "Fizz"
    | i `mod`  5 == 0 = "Buzz"
    | otherwise       = show i

main = do
  traverse_ (putStrLn . showValue) [1..100]
