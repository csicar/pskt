package Foreign.Data.Ord;

fun curry5(f: (Any, Any, Any, Any, Any) -> Any) = {a: Any -> { b: Any -> { c: Any -> {d: Any -> {e: Any ->
    f(a, b, c, d, e)
}}}}}

val ordBooleanImpl = curry5 { lt, eq, gt, x, y ->
    when((x as Boolean).compareTo(y as Boolean)) {
        -1 -> lt
        0 -> eq
        else -> gt
    }
}

val ordIntImpl = curry5 { lt, eq, gt, x, y ->
    x as Int; y as Int
    when {
        x < y -> lt
        x == y -> eq
        else -> gt
    }
}

val ordNumberImpl = curry5 { lt, eq, gt, x, y ->
    x as Double; y as Double
    val x1 = x.toDouble()
    val y1 = y.toDouble()
    when {
        x1 < y1 -> lt
        x1 == y1 -> eq
        else -> gt
    }
}

val ordStringImpl = curry5 { lt, eq, gt, x, y ->
    x as String; y as String
    when {
        x < y -> lt
        x == y -> eq
        else -> gt
    }
}


val ordCharImpl = curry5 { lt, eq, gt, x, y ->
    x as Char; y as Char
    when {
        x < y -> lt
        x == y -> eq
        else -> gt
    }
}

val ordArrayImpl = { f: Any ->
    { xs: Any ->
        { ys: Any ->
            xs as List<Any>; ys as List<Any>; f as (Any) -> ((Any) -> Any)
            fun cmp(): Any {
                var i = 0;
                val xlen = xs.size
                val ylen = ys.size
                while (i < xlen && i < ylen) {
                    val o = f(xs[i])(ys[i])
                    if (o != 0) {
                        return o
                    }
                    i++
                }
                return when {
                    xlen == ylen -> 0
                    xlen > ylen -> -1
                    else -> 1
                }
            }
            cmp()
        }
    }
}