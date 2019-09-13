module Main where

import Test.HUnit
import CodeGen.CoreImp
import CLI
import System.Process
import System.Directory


tests = TestCase $ do
  system "whereis purs"
  withCurrentDirectory "./test" $ 
    system "spago build -- --codegen corefn"
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
    system "./gradlew fatJar"
    stdout <- readProcess "java" ["-jar", "build/libs/test-1.0-SNAPSHOT-fat.jar"] ""
    putStrLn stdout
    assertEqual "output should match" expectedOutput stdout
  

main :: IO ()
main = do
  runTestTT tests
  return ()