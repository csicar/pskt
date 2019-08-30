package Foreign.Global;

val nan = Double.NaN

val isNaN = { x: Any -> x as Double; x.isNaN() }

val infinity = Double.POSITIVE_INFINITY

val isFinite = { x: Any -> x as Double; x.isFinite() }

val readInt = { radix: Any -> { n: Any -> radix as Int; n as String; n.toInt(radix) } }

val readFloat = { x:Any -> x as String; x.toDouble()}