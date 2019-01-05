#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.8
  --package attoparsec
  --package bytestring
  --package time
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time hiding (parseTime)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse
             | Keyboard
             | Monitor
             | Speakers
             deriving Show

data LogEntry = LogEntry
    { entryTime    :: LocalTime
    , entryIP      :: IP
    , entryProduct :: Product
    } deriving Show

type Log = [LogEntry]


--parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ show $ IP d1 d2 d3 d4


--parseDay :: Parser Day
parseDay = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  return $ show $ fromGregorian (read y) (read mm) (read d)


--parseTime :: Parser TimeOfDay
parseTime = do
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $ show $ TimeOfDay (read h) (read m) (read s)


parseProduct :: Parser Product
parseProduct = undefined

parseLogEntry :: Parser LogEntry
parseLogEntry = undefined

parseLog :: Parser Log
parseLog = undefined


ip   = "127.0.0.1"
day  = "2013-06-29"
time = "12:52:17"


testParser (p,bs) = do
  let parse_result = parseOnly p bs
  case parse_result of
    Left err -> print err
    Right res -> putStrLn res

main :: IO ()
main = do
  let testPairs = [ (parseIP, ip)
                  , (parseDay, day)
                  , (parseTime, time)
                  ]
  forM_ testPairs testParser
