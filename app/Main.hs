{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (print)
import Control.Monad (when)
import qualified Control.Monad.Parallel as Par
import Data.Aeson
import Data.Aeson.Types hiding (Parser)
import Data.Char
import Data.Foldable (for_)
import Data.FileEmbed (embedFile)
import Data.List (delete, intercalate, isPrefixOf, nub, partition)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Version
import Control.Monad.Supply
import Control.Monad.Supply.Class
import Text.Printf
import System.FilePath.Glob as G
import qualified Data.Text.Lazy.IO as TIO

import System.Environment
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime)
import System.FilePath ((</>), joinPath, searchPathSeparator, splitDirectories, takeDirectory)
import System.Process

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B

import Development.GitRev

import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.Names (runModuleName)
import CodeGen.CoreImp
import CodeGen.KtCore
import CodeGen.Printer
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import System.IO (openFile, IOMode(..), hClose, print)
import Shelly (cp_r, shelly)
import Filesystem.Path.CurrentOS (decodeString)

import Options.Applicative hiding (Success)

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

data CliOptions = CliOptions
  { inputFiles :: [FilePath]
  , foreignDirs :: [FilePath]
  , outputDir :: FilePath
  , printCoreFn :: Bool
  }

cli :: Parser CliOptions
cli = CliOptions
  <$> many 
    ( strOption 
      ( long "input"
      <> short 'i'
      <> metavar "FILENAME"
      )
    )
  <*> many
    ( strOption
      ( long "foreign import"
      <> short 'f'
      <> metavar "FILENAME"
      )
    )
  <*> strOption
    ( long "outputDir"
    <> short 'o'
    <> metavar "FILENAME"
    <> value "kotlin/"
    )
  <*> switch
    ( long "print-corefn"
    <> help "print debug info about read corefn"
    )

-- Adding program help text to the parser
optsParserInfo :: ParserInfo CliOptions
optsParserInfo = info (cli <**> helper)
  (  fullDesc
  <> progDesc "pskt"
  <> header "PureScript Transpiler to Kotlin using CoreFn"
  )

main :: IO ()
main = do
  putStrLn "pskt"
  opts <- execParser optsParserInfo
  putStrLn "parsed"
  let files = inputFiles opts
  let outputPath = outputDir opts
  putStrLn "input:"
  foundInputFiles <- G.globDir (G.compile <$> files) "./"
  print foundInputFiles
  print outputPath
  addRuntime outputPath
  print $ foreignDirs opts
  for_ (foreignDirs opts) $ \folder -> shelly (cp_r (decodeString folder) (decodeString outputPath))
  for_ (concat foundInputFiles) $ \file -> processFile opts outputPath file

addRuntime :: FilePath -> IO ()
addRuntime folder = do
  file <- openFile (folder </> "PSRuntime.kt") WriteMode
  TIO.hPutStr file $ L.pack $ concat [
      "package Foreign.PsRuntime;",
      "",
      "fun Any.app(arg: Any): Any {",
          "return (this as (Any) -> Any)(arg)",
      "}"
    ]
  hClose file

processFile :: CliOptions -> FilePath -> FilePath -> IO ()
processFile opts outputDirPath path = do
  jsonText <- T.decodeUtf8 <$> B.readFile path
  let mod = jsonToModule $ parseJson jsonText
  let modName = runModuleName $ moduleName mod
  if printCoreFn opts then pPrint mod else pure ()
  let moduleKt = moduleToKt' mod
  -- pPrint moduleKt
  outputFile <- openFile (outputDirPath </> T.unpack modName <> ".kt") WriteMode
  let moduleDoc = moduleToText mod
  renderIO outputFile moduleDoc
  hClose outputFile
  TIO.putStrLn $ renderLazy moduleDoc
