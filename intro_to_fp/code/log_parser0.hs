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
import Data.Time
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

parseIP :: Parser IP
parseIP = undefined

parseDay :: Parser Day
parseDay = undefined

parseTime :: Parser TimeOfDay
parseTime = undefined

parseProduct :: Parser Product
parseProduct = undefined

parseLogEntry :: Parser LogEntry
parseLogEntry = undefined

parseLog :: Parser Log
parseLog = undefined


main :: IO ()
main = return ()
