package Foreign.Data.Semiring;

val intAdd = { x: Any -> { y: Any -> 
  x as Int; y as Int
  x + y
}}
val intMul = { x: Any -> { y: Any -> 
  x as Int; y as Int
  x * y
}}
val numAdd = { x: Any -> { y: Any -> 
  x as Double; y as Double
  x + y
}}
val numMul = { x: Any -> { y: Any -> 
  x as Double; y as Double
  x * y
}}