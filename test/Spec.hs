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

foreignsDirectory = "./kotlin/src/main/kotlin/foreigns"

tests = do
  withDefaultPath "whereis purs"
  foreignsExists <- doesDirectoryExist foreignsDirectory
  if foreignsExists then
    withCurrentDirectory foreignsDirectory $ void $ system "git pull"
  else 
    system' "git clone https://github.com/csicar/pskt-foreigns kotlin/src/main/kotlin/foreigns"
  withCurrentDirectory "./test" $ do
    withDefaultPath "spago build -- --codegen corefn"
    _ <- compile $ CliOptions
      { printCoreFn = False
      , printTranspiled = False
      , printVersion = True
      }
    putStrLn "compiled"
    hFlush stdout
  system' "ln -s -f $(pwd)/test/output/pskt/ ./kotlin/src/main/kotlin/generated"
  expectedOutput <- readFile "./test/src/Main.txt"
  withCurrentDirectory "./kotlin" $ do
    system' "JAVA_HOME=/usr/lib/jvm/default gradle fatJar"
    stdout <- readProcess "java" ["-jar", "build/libs/test-1.0-SNAPSHOT-fat.jar"] ""
    putStrLn stdout
    assertEqual "output should match" expectedOutput stdout
  

main :: IO ()
main = tests