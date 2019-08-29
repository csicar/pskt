package Foreign.Data.HeytingAlgebra;

val boolConj = { b1: Any -> { b2: Any -> 
  b1 as Boolean; b2 as Boolean
  b1 && b2
}}

val boolDisj = { b1: Any -> { b2: Any -> 
  b1 as Boolean; b2 as Boolean  
  b1 || b2
}}

val boolNot = { b: Any ->
  b as Boolean
  b.not()
}