package Foreign.Data.EuclideanRing;

import kotlin.math.abs
import kotlin.math.floor
import kotlin.math.min


val intDegree = { x: Any ->
    x as Int
   min(abs(x.toLong()), 2147483647).toInt();
}

val intDiv = { x: Any ->
   { y: Any ->
       x as Int; y as Int
       when {
           y == 0 -> 0
           y > 0 -> x.div(y)
           else -> - x.div(-y)
       }
  }
}

val intMod = { x: Any ->
   { y: Any ->
       x as Int; y as Int
       val yy = abs(y)
       when {
           y==0 -> 0
           else -> ((x % yy) + yy) % yy
       }
  }
}

val numDiv = { n1: Any ->
   { n2: Any ->
       n1 as Double; n2 as Double
     n1 / n2;
  }
}