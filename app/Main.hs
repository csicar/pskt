module Main where

import Options.Applicative hiding (Success)

import CLI

main :: IO ()
main = do
  putStrLn "pskt start:"
  opts <- execParser optsParserInfo
  putStrLn "parsed"
  compile opts
