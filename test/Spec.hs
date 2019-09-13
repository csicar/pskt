module Main where

import Test.HUnit
import CodeGen.CoreImp
import CLI
import System.Process
import System.Directory
import System.Exit

system' cmd = do
  code <- system cmd
  case code of
    ExitSuccess -> return ()
    ExitFailure a -> fail $ "Command failed with: " ++ show a

withDefaultPath cmd = 
  system' $ "bash -c 'PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin; export PATH; "<> cmd <>"'"

tests = TestCase $ do
  withDefaultPath "whereis purs"
  system' "echo version:"
  system' "purs --version"
  system' "echo $PATH"
  withCurrentDirectory "./test" $ withDefaultPath "spago build -- --codegen corefn"
  compile $ CliOptions
    { inputFiles = ["test/output/*/corefn.json"]
    , foreignDirs = ["../foreigns/*.kt"]
    , outputDir = "./kotlin/src/main/kotlin/generated"
    , printCoreFn = False
    , printTranspiled = False
    }
  putStrLn "compiled"
  expectedOutput <- readFile "./test/src/Main.txt"
  withCurrentDirectory "./kotlin" $ do
    system' "./gradlew fatJar"
    stdout <- readProcess "java" ["-jar", "build/libs/test-1.0-SNAPSHOT-fat.jar"] ""
    putStrLn stdout
    assertEqual "output should match" expectedOutput stdout
  

main :: IO ()
main = do
  runTestTT tests
  return ()