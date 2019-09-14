module Main where

import Options.Applicative hiding (Success)
import Prelude
import CLI

main :: IO ()
main = do
  putStrLn "PsKt transpile:"
  opts <- execParser optsParserInfo
  compile opts
  putStrLn "done"