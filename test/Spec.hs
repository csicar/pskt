import CodeGen.CoreImp
import CLI
import System.Process
import System.Directory
import System.Exit
import System.IO
import Data.Functor (void)
import           Test.Hspec

system' cmd = do
  code <- system cmd
  case code of
    ExitSuccess -> return ()
    _ -> exitWith code

withDefaultPath cmd = 
  system' $ "bash -c 'PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin; export PATH; "<> cmd <>"'"

foreignsDirectory = "./foreigns"
foreignsDirectoryGlobal = "./test/foreigns"

spec :: Spec
spec = 
  describe "intergration test" $
    it "compile" $ do
      withDefaultPath "whereis purs"
      foreignsExists <- doesDirectoryExist foreignsDirectoryGlobal
      if foreignsExists then
        withCurrentDirectory foreignsDirectoryGlobal $ void $ system "git pull"
      else 
        system' $ "git clone https://github.com/csicar/pskt-foreigns " <> foreignsDirectoryGlobal
      withCurrentDirectory "./test" $ do
        system "rm -r output"
        withDefaultPath "spago build --purs-args --codegen --purs-args corefn"
        _ <- compile $ CliOptions
          { printCoreFn = False
          , printTranspiled = False
          , printVersion = True
          , runProgram = True
          , foreigns = [foreignsDirectory, "src/"]
          }
        putStrLn "compiled"
        hFlush stdout
        stdout <- readProcess "java" ["-jar", "output/pskt/program.jar"] ""
        putStrLn stdout
        expectedOutput <- readFile "./src/Main.txt"
        expectedOutput `shouldBe` stdout
        
main = hspec spec