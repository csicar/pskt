package Foreign.Data.Ring;

val intSub = { x : Any -> { y: Any ->
  x as Int; y as Int
  x - y
}}

val numSub = { n1: Any -> { n2: Any ->
  n1 as Double; n2 as Double
  n1 - n2
}}