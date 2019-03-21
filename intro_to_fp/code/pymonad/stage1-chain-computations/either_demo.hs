#!/usr/bin/env stack
{- stack script --resolver lts-13.8 -}

type Error = String

ok_computation :: Either Error Int
ok_computation = Right(1) >> Right(2) >> Right(3) >> Right(4)

nok_computation :: Either Error Int
nok_computation = Right(1) >> Right(2) >> Left("Something bad happened") >> Right(4)

main = do
  print ok_computation
  print nok_computation
