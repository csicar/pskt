package Foreign.Data.Semigroup;

val concatString = { x: Any -> 
  { y: Any -> 
    x as String; y as String
    x + y
  }
}

val concatArray = { x: Any ->
  { y: Any -> 
    x as List<Any>; y as List<Any>

    x.plus(y)
  }
}