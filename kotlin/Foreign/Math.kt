package Foreign.Math;


import kotlin.math.pow as ktPow

val abs = { x: Any -> x as Double; kotlin.math.abs(x) }

val acos = { x: Any -> x as Double; kotlin.math.acos(x) }

val asin = { x: Any -> x as Double; kotlin.math.asin(x) }

val atan = { x: Any -> x as Double; kotlin.math.atan(x) }

val atan2 = {x: Any -> {y : Any ->
  x as Double; y as Double
  kotlin.math.atan2(x, y)
}}

val ceil = {x: Any -> x as Double; kotlin.math.ceil(x) }

val cos = { x: Any -> x as Double; kotlin.math.cos(x) }

val exp = { x: Any -> x as Double; kotlin.math.exp(x) }

val floor = { x: Any -> x as Double; kotlin.math.floor(x) }

val trunc = {x : Any -> x as Double; kotlin.math.truncate(x)}

val log = {x : Any -> x as Double; kotlin.math.ln(x) }

val max = { n1: Any ->
  { n2: Any ->
    n1 as Double; n2 as Double
    kotlin.math.max(n1, n2);
  }
}

val min = { n1: Any ->
  { n2: Any ->
    n1 as Double; n2 as Double
    kotlin.math.min(n1, n2);
  }
}

val pow = { n: Any ->
  { p: Any ->
    n as Double; p as Double
    n.ktPow(p)
  }
}

val remainder = { n: Any ->
  { m: Any ->
    n as Double; m as Double
    n % m;
  };
};

val round = { x: Any -> x as Double; kotlin.math.round(x) }

val sin = { x: Any -> x as Double; kotlin.math.sin(x) }

val sqrt = { x: Any -> x as Double; kotlin.math.sqrt(x) }

val tan = { x: Any -> x as Double; kotlin.math.tan(x) }

const val e = Math.E;

val ln2 = kotlin.math.ln(2.0)

val ln10 = kotlin.math.ln(10.0)

val log2e = kotlin.math.log2(Math.E)

val log10e = kotlin.math.log10(Math.E)

val pi = Math.PI;

val tau = 2 * Math.PI;

val sqrt1_2 = kotlin.math.sqrt(0.5);

val sqrt2 = kotlin.math.sqrt(2.0);