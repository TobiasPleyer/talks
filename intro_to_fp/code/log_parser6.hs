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
import Control.Monad (forM_)
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


parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4


parseDay :: Parser Day
parseDay = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  return $ fromGregorian (read y) (read mm) (read d)


parseTime :: Parser TimeOfDay
parseTime = do
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $ TimeOfDay (read h) (read m) (read s)


parseProduct :: Parser Product
parseProduct =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)


parseLogEntry :: Parser LogEntry
parseLogEntry = do
  d <- parseDay
  char ' '
  t <- parseTime
  char ' '
  ip <- parseIP
  char ' '
  p <- parseProduct
  return $ LogEntry (LocalTime d t) ip p


parseLog :: Parser Log
parseLog = many $ parseLogEntry <* endOfLine


main :: IO ()
main = do
  let logFile = "example.log"
  log_content <- B.readFile logFile
  let parse_result = parseOnly parseLog log_content
  case parse_result of
    Left err -> print err
    Right logs -> forM_ logs print
