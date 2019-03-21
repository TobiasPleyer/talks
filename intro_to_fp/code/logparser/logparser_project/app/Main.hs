module Main where

import Control.Monad (forM_)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Lib

main :: IO ()
main = do
  let logFile = "../example.log"
  log_content <- B.readFile logFile
  let parse_result = parseOnly parseLog log_content
  case parse_result of
    Left err -> print err
    Right logs -> forM_ logs print
