module Main where

import Test.HUnit
import CodeGen.CoreImp
import CLI
import System.Process
import System.Directory
import System.Exit
import System.IO
import Data.Functor (void)

system' cmd = do
  code <- system cmd
  case code of
    ExitSuccess -> return ()
    _ -> exitWith code

withDefaultPath cmd = 
  system' $ "bash -c 'PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin; export PATH; "<> cmd <>"'"

foreignsDirectory = "./foreigns"
foreignsDirectoryGlobal = "./test/foreigns"

tests = do
  withDefaultPath "whereis purs"
  foreignsExists <- doesDirectoryExist foreignsDirectoryGlobal
  if foreignsExists then
    withCurrentDirectory foreignsDirectoryGlobal $ void $ system "git pull"
  else 
    system' $ "git clone https://github.com/csicar/pskt-foreigns " <> foreignsDirectoryGlobal
  withCurrentDirectory "./test" $ do
    system "rm -r output"
    withDefaultPath "spago build -- --codegen corefn"
    _ <- compile $ CliOptions
      { printCoreFn = False
      , printTranspiled = False
      , printVersion = True
      , runProgram = True
      , foreigns = [foreignsDirectory]
      }
    putStrLn "compiled"
    hFlush stdout
    expectedOutput <- readFile "./src/Main.txt"
    stdout <- readProcess "java" ["-jar", "output/pskt/program.jar"] ""
    putStrLn stdout
    assertEqual "output should match" expectedOutput stdout
  

main :: IO ()
main = tests