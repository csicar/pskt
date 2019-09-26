module Main where

-- import Data.Unit
-- import Data.Show
-- import Data.Semigroup ((<>))
-- import Data.Semiring ((+), (*))
-- import Control.Semigroupoid ((>>>))
-- import Data.Function (const)
-- g x | true = 1+2
import Prelude
import Effect.Console (log, logShow)
import Data.Foldable
import Effect
import Data.Array (uncons)
import Data.Maybe
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

composeTest x = (_ + 1) >>> (_ * 8983)

f = [12, 2,3] <> [2]

fac 0 = 1
fac n = n* fac (n - 1)

testCases [1, 2, 3] = 1
testCases [a] = a
testCases a | any (_ > 2) a = 1337
testCases a = 0

testCaseRecord {label1: "2", label2: b} = "first " <> b
testCaseRecord {label1: a, label2: "6"} = "snd " <> a
testCaseRecord {label1: a, label2: b} = "thrd " <> a <> b

mutualRecursion a | a < 0 = 0
mutualRecursion a = a + mutualRecursionB (a-1)

mutualRecursionB a = a * mutualRecursion (a - 1)

testLocalRecursion ls = go ls 0
  where
    go ls acc = case uncons ls of
      Just {head, tail} -> go tail $ acc + head
      Nothing -> acc

testLocalBinds = f a
  where
    f x = x * 2
    a = 23 * 8

magicDoPure :: Effect Int
magicDoPure = pure 4

-- ST test
stTest :: Int
stTest = ST.run do
  total <- STRef.new 0
  let loop 0 = STRef.read total
      loop n = do
        _ <- STRef.modify (_ + (n * n)) total
        loop (n - 1)
  loop 100

main = do
  log "test unicode:"
  log "start 😁 😂 🤣 😃 😄 😅 end $ ' \""
  log "test recursion:"
  logShow $ fac 10
  logShow f
  log "composeTest:"
  logShow $ composeTest 2 2
  log "test cases:"
  logShow $ testCases [1, 2, 3]
  logShow $ testCases [99]
  logShow $ testCases [3, -2]
  logShow $ testCases [-2, -3]
  logShow $ testCaseRecord {label1: "a", label2: "llll"}
  logShow $ testCaseRecord {label1: "2", label2: "usll"}
  logShow $ testCaseRecord {label1: "3283", label2: "6"}
  log "mutual recursion:"
  logShow $ mutualRecursion 5
  log "test local recursion"
  logShow $ testLocalRecursion [1, 2, 3, -2, 4]
  log "testLocalBinds:"
  logShow $ testLocalBinds
  log "test magicDo"
  val <- magicDoPure
  logShow val
  log "test ST"
  logShow stTest
  log "test inline"
  logShow ( (<>) [1, 2] [3])
  logShow (1 + 2)
  logShow (1 * 2 *3)
  logShow $ true && false
  logShow $ true || false
  log "test records"
  logShow {}
  logShow {a: 2, b: 3}
  logShow ({a: 2, b: 3} {b = 0} )
  pure 1