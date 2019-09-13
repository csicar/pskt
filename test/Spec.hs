module Main where

import Test.HUnit
import CodeGen.CoreImp
import CLI
import System.Process
import System.Directory
import System.Exit
import System.IO

system' cmd = do
  code <- system cmd
  case code of
    ExitSuccess -> return ()
    _ -> exitWith code

withDefaultPath cmd = 
  system' $ "bash -c 'PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin; export PATH; "<> cmd <>"'"

tests = do
  withDefaultPath "whereis purs"
  withCurrentDirectory "./test" $ withDefaultPath "spago build -- --codegen corefn"
  system' "git clone https://github.com/csicar/pskt-foreigns kotlin/foreigns"
  _ <- compile $ CliOptions
    { inputFiles = ["test/output/*/corefn.json"]
    , foreignDirs = ["./kotlin/foreigns/*.kt"]
    , outputDir = "./kotlin/src/main/kotlin/generated"
    , printCoreFn = False
    , printTranspiled = False
    }
  hFlush stdout
  putStrLn "compiled"
  expectedOutput <- readFile "./test/src/Main.txt"
  withCurrentDirectory "./kotlin" $ do
    system' "JAVA_HOME=/usr/lib/jvm/default gradle fatJar"
    stdout <- readProcess "java" ["-jar", "build/libs/test-1.0-SNAPSHOT-fat.jar"] ""
    putStrLn stdout
    assertEqual "output should match" expectedOutput stdout
  

main :: IO ()
main = tests