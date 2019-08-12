module Test where

-- a = "as"

-- b = 123.0

-- c = [1, 2]

-- f x y reallyLongName reallyLongName2 reallyLongName3 = x y

-- testVal = f (\x -> 32) (23)
foreign import log :: String -> String

data Ty = A Int | B String

test = A 23

class Test a b where
  doStuff :: a -> b -> String

main x = do
  log "asd"