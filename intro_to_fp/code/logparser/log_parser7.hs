#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.8
  --package attoparsec
  --package base
  --package bytestring
  --package time
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, toLower)
import Data.Word
import Data.Time hiding (parseTime)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)


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


word8 :: Parser Word8
word8 = do
  num <- decimal :: Parser Integer
  if num < 256
  then return (fromIntegral num)
  else fail "Field must be in the range [0-255]."


parseIP :: Parser IP
parseIP = do
  d1 <- word8
  char '.'
  d2 <- word8
  char '.'
  d3 <- word8
  char '.'
  d4 <- word8
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
parseProduct = do
  product <- map toLower <$> many (satisfy isAlpha)
  case product of
    "mouse" -> return Mouse
    "keyboard" -> return Keyboard
    "monitor" -> return Monitor
    "speakers" -> return Speakers
    _ -> fail ("Unknown product " ++ product)


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
parseLog = do
  log <- many $ parseLogEntry <* endOfLine
  end <- atEnd
  if not end
  then fail ("Error in line #" ++ show (length log + 1))
  else return log


main :: IO ()
main = do
  logFile <- head <$> getArgs
  log_content <- B.readFile logFile
  -- Try to parse every line successfully or fail
  let parse_result = parseOnly parseLog log_content
  case parse_result of
    Left err -> print err
    Right logs -> forM_ logs print
  -- Parse every line and continue on fail
  let lineParses = map (parseOnly parseLogEntry) (BC.lines log_content)
  forM_ lineParses print
