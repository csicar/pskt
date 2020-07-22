{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module CLI where

import Version
import Prelude hiding (print)
import Control.Monad (when)
import qualified Control.Monad.Parallel as Par
import Data.Aeson
import Data.Aeson.Types hiding (Parser)
import Data.Char
import Data.Foldable (for_)
import Data.Traversable (forM)
import Data.FileEmbed (embedFile)
import Data.List (delete, intercalate, isPrefixOf, nub, partition)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Version
import Control.Monad.Supply
import Control.Monad.Supply.Class
import Text.Printf
import qualified System.FilePath.Glob as G
import qualified Data.Text.Lazy.IO as TIO

import System.Environment
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime)
import System.FilePath ((</>), takeFileName, joinPath, searchPathSeparator, splitDirectories, takeDirectory)
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
import Protolude (unsnoc)
import Data.List.Extra (split, notNull)

import Options.Applicative (many, auto, argument, showDefault, metavar, help, option, long, short, Parser, ParserInfo, header, progDesc, fullDesc, helper, info, switch, value, str, (<**>))

import Text.Pretty.Simple (pPrint)
import Version

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

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
  { printVersion :: Bool
  , printCoreFn :: Bool
  , printTranspiled :: Bool
  , runProgram :: String
  , foreigns :: [FilePath]
  }

cli :: Parser CliOptions
cli = CliOptions
  <$> switch
    ( long "print-version"
    <> short 'v'
    <> help "print PsKT version"
    )
  <*> switch
    ( long "print-corefn"
    <> help "print debug info about read corefn"
    )
  <*> switch
    ( long "print-transpiled"
    <> help "print debug info about transpiled files"
    )
  <*> option str
    ( long "run"
    <> help "also run the transpiled program: The entry point needs to have type `:: Effect _`. Example: `-- run Main.main`"
    <> value ""
    )
  <*> many
    ( option str
      ( long "foreigns"
      <> help "folders containing foreign files. The matching files are passed to kotlinc and copied to the `output` folder"
      )
    )

-- Adding program help text to the parser
optsParserInfo :: ParserInfo CliOptions
optsParserInfo = info (cli <**> helper)
  (  fullDesc
  <> progDesc ("pskt " <> versionString)
  <> header "PureScript Transpiler to Kotlin using CoreFn"
  )

shakeOpts = shakeOptions
  { shakeFiles="output/pskt"
  , shakeProgress = progressSimple
  , shakeThreads = 0 -- automatically choose number of threads
  , shakeVersion = versionString
  }

getModuleNames = fmap (takeFileName . takeDirectory) <$> getDirectoryFiles "" ["output/*/corefn.json"]

compile :: CliOptions -> IO ()
compile opts = shake shakeOpts $ do
  action $ do
    when (printVersion opts) $ putNormal $ "PsKt Version: " <> versionString
    cs <- getModuleNames
    let kotlinFiles = ["output/pskt" </>  c <.> "kt" | c <- cs]
    need ["foreigns"]
    need ["output/pskt/PsRuntime.kt"]
    need kotlinFiles
    case runProgram opts of
      "" -> pure ()
      mainMod -> do
        need ["output/pskt/entryPoint" </> mainMod <.> "kt"]
        let jarFile = "output/pskt" </> mainMod <.> "jar"
        need [jarFile]
        command_ [] "java" ["-jar", jarFile]

  "output/pskt/*.jar" %> \out -> do
    let modName = takeBaseName out
    ktFiles <- fmap (\mod -> "output/pskt" </> mod <.> "kt") <$> getModuleNames
    need ktFiles
    need ["output/pskt/entryPoint" </> modName <.> "kt"]
    command_
      [AddEnv "JAVA_OPTS" "-Xmx2G -Xms256M"]
      "kotlinc" $
        ["output/pskt/PsRuntime.kt", "output/pskt/entryPoint" </> modName <.>"kt"] 
        ++ ktFiles 
        ++ ["output/pskt/foreigns" ]
        ++ ["-include-runtime", "-d", out]
        ++ ["-nowarn"]
  
  "output/pskt/entryPoint/*.kt" %> \out -> do
    let modName = takeBaseName out
    let Just (start, end) = unsnoc $ split (=='.') $ modName
    writeFileLines out
          [ "@file:Suppress(\"UNCHECKED_CAST\", \"USELESS_CAST\")"
          , "import Foreign.PsRuntime.appRun;"
          , ""
          , "fun main() {"
          , "   PS."<> intercalate "." start <> ".Module."<> end <> ".appRun()"
          , "}"
          ]

  "output/pskt/PsRuntime.kt" %> \out ->
    writeFileChanged out $ unlines
      [ "@file:Suppress(\"UNCHECKED_CAST\", \"USELESS_CAST\")"
      , "package Foreign.PsRuntime;"
      , ""
      , "fun Any.app(arg: Any): Any {"
      , "   return (this as (Any) -> Any)(arg)"
      , "}"
      , ""
      , "fun Any.appRun() = (this as () -> Any)()"
      ]

  phony "foreigns" $ do
    let foreignOut = "output/pskt/foreigns/"
    foreignFiles <- forM (foreigns opts) $ \folder -> do
      files <- getDirectoryFiles folder ["*.kt"]
      return $ (\file -> (fileToModule file, folder </> file)) <$> files
    for_ (concat foreignFiles) $ \(modName, file) ->
      copyFileChanged file (foreignOut </> modName <.> "kt")
      

  "output/pskt/*.kt" %> \out -> do
    let modName = takeBaseName out
    processFile opts out ("output" </> modName </> "corefn.json")

fileToModule :: FilePath -> String
fileToModule path = replaceSlash <$> path
    where 
      replaceSlash '/' = '.'
      replaceSlash char = char

processFile :: CliOptions -> FilePath -> FilePath -> Action ()
processFile opts outFile path = do
  jsonText <- T.pack <$> readFile' path
  let mod = jsonToModule $ parseJson jsonText
  let modName = runModuleName $ moduleName mod
  if printCoreFn opts then pPrint mod else pure ()
  let moduleKt = moduleToKt' mod
  -- pPrint moduleKt
  outputFile <- liftIO $ openFile outFile WriteMode
  putNormal $ "Transpiling " <> T.unpack modName
  let moduleDoc = moduleToText mod
  liftIO $ renderIO outputFile moduleDoc
  liftIO $ hClose outputFile
  liftIO $ if printTranspiled opts then TIO.putStrLn $ renderLazy moduleDoc else pure ()
