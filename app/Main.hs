{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (print)
import Control.Monad (when)
import qualified Control.Monad.Parallel as Par
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.FileEmbed (embedFile)
import Data.List (delete, intercalate, isPrefixOf, nub, partition)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Version
import Control.Monad.Supply
import Control.Monad.Supply.Class
import Text.Printf

import System.Environment
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime)
import System.FilePath ((</>), joinPath, searchPathSeparator, splitDirectories, takeDirectory)
import System.FilePath.Find
import System.Process

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B

import Development.GitRev

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import CodeGen.CoreImp
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import System.IO (openFile, IOMode(..), hClose)


import Text.Pretty.Simple (pPrint)

parseJson :: Text -> Value
parseJson text
  | Just fileJson <- decode . L.encodeUtf8 $ L.fromStrict text = fileJson
  | otherwise = error "Bad json"

jsonToModule :: Value -> Module Ann
jsonToModule value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> error "failed"

main :: IO ()
main = do
  [path, outputPath] <- getArgs
  jsonText <- T.decodeUtf8 <$> B.readFile path
  let mod = jsonToModule $ parseJson jsonText
  pPrint $ mod
  outputFile <- openFile outputPath WriteMode
  renderIO outputFile (moduleToText mod)
  hClose outputFile
  putDocW 80 $ print mod