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

composeTest x = (_ + 1) >>> (_ * 8983)

f = [12, 2,3] <>[2]

fac 0 = 1
fac n = n* fac (n - 1)

main = do
  log "asd"
  log "😘kkk"
  logShow $ fac 10
  logShow f
  log "composeTest:"
  logShow $ composeTest 2 2
  pure 1