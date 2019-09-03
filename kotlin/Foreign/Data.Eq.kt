package Foreign.Data.Eq;

val refEq = { r1 : Any -> { r2 : Any -> r1 == r2 }}

val eqBooleanImpl = refEq;
val eqIntImpl = refEq;
val eqNumberImpl = { r1: Any ->
  { r2: Any ->
    // casting is necessary to ensure, because Object Comparison does not work for NaN
    r1 as Double; r2 as Double
    r1.toDouble() == r2.toDouble()
  }
  
};
val eqCharImpl = refEq;
val eqStringImpl = refEq;
val eqArrayImpl = { f: Any ->
  { xs: Any ->
    { ys: Any ->
      xs as List<Any>; ys as List<Any>; f as (Any) -> ((Any) -> Boolean)
      if (xs.size != ys.size) {
        false
      } else {
        xs.zip(ys).all { (x, y) -> f(x)(y) }
      }
    }
  }
};