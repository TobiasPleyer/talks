{-# LANGUAGE OverloadedStrings #-}
module Lib
where


import Data.Attoparsec.ByteString.Char8
import Data.Time hiding (parseTime)
import Data.Word


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
