package Foreign.Data.Array


val range = { start: Any ->
    { end: Any ->
        start as Int; end as Int
        start.rangeTo(end)
    }
}

var replicate = { count: Any ->
    { value: Any ->
        count as Int
        List(count) { value }
    }
}


val fromFoldableImpl = { foldr: Any ->
    { xs: Any ->
        foldr as ((Any) -> ((Any) -> (Any) -> Any))
        foldr { a: Any ->
            { b: Any ->
                listOf(a) + b
            }
        }(emptyList<Any>())(xs)
    }
}

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

val length = { xs: Any ->
    xs as List<Any>
    xs.size
}

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------

val cons = { e: Any ->
    { l: Any ->
        e; l as List<Any>
        listOf(e) + l
    }
}

val snoc = { l: Any ->
    { e: Any ->
        l as List<Any>
        l + listOf(e)
    }
}

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

val uncons_tick = { empty: Any ->
    { next: Any ->
        { xs: Any ->
            empty as (Any) -> Any; next as (Any) -> ((List<Any>) -> Any); xs as List<Any>
            if (xs.size == 0) {
                empty(Unit)
            } else {
                next(xs[0])(xs.subList(1, xs.size))
            }
        }
    }
}

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

val indexImpl = { just: Any ->
    { nothing: Any ->
        { xs: Any ->
            { i: Any ->
                just as (Any) -> Any; i as Int; xs as List<Any>
                if (i < 0 || i >= xs.size) {
                    nothing
                } else {
                    just(xs[i])
                }
            }
        }
    }
}

val findIndexImpl = { just: Any ->
    { nothing: Any ->
        { f: Any ->
            { xs: Any ->
                just as (Any) -> Any; f as (Any) -> Boolean; xs as List<Any>
                val mIndex = xs.withIndex().find {
                    f(it.value)
                }
                if (mIndex == null) {
                    nothing
                } else {
                    just(mIndex)
                }
            }
        }
    }
}

val findLastIndexImpl = { just: Any ->
    { nothing: Any ->
        { f: Any ->
            { xs: Any ->
                just as (Any) -> Any; f as (Any) -> Boolean; xs as List<Any>
                val mIndex = xs.withIndex().findLast {
                    f(it.value)
                }
                if (mIndex == null) {
                    nothing
                } else {
                    just(mIndex)
                }
            }
        }
    }
}

val __insertAt = { just: Any ->
    { nothing: Any ->
        { i: Any ->
            { a: Any ->
                { l: Any ->
                    i as Int; just as (Any) -> Any; l as List<Any>
                    if (i < 0 || i > l.size) {
                        nothing
                    } else {

                        var l1 = l.toMutableList()
                        l1.add(i, a)
                        just(l1.toList())
                    }
                }
            }
        }
    }
}

val __deleteAt = { just: Any ->
    { nothing: Any ->
        { i: Any ->
            { l: Any ->
                i as Int; l as List<Any>
                if (i < 0 || i > l.size) {
                    nothing
                } else {
                    just as (Any) -> Any
                    just(l.filterIndexed { index, x -> index != i })
                }
            }
        }
    }
}


val __updateAt = { just: Any ->
    { nothing: Any ->
        { i: Any ->
            { a: Any ->
                { l: Any ->
                    i as Int; l as List<Any>
                    if (i < 0 || i > l.size) {
                        nothing
                    } else {
                        just as (Any) -> Any
                        just(l.mapIndexed { index, any ->
                            index != i
                            when (index) {
                                i -> a
                                else -> any
                            }
                        })
                    }
                }
            }
        }
    }
}

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

val reverse = { l: Any ->
    l as List<Any>
    l.reversed()
}

val concat = { xss: Any ->
    xss as List<List<Any>>
    xss.flatten()
}

val filter = { f: Any ->
    { xs: Any ->
        xs as List<Any>; f as (Any) -> Boolean
        xs.filter(f)
    }
}

val partition = { f: Any ->
    { xs: Any ->
        xs as List<Any>; f as (Any) -> Boolean
        val (yes, no) = xs.partition { f(it) }
        mapOf("yes" to yes, "no" to no)
    }
}

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

val sortImpl = { f: Any ->
    { l: Any ->
        f as ((Any) -> ((Any) -> Int)); l as List<Any>
        val ml = l.toMutableList()
        ml.sortWith(Comparator { a, b -> f(a)(b)})
        ml.toList()
    }
}

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

val slice = { s: Any ->
    { e: Any ->
        { l: Any ->
            l as List<Any>; s as Int; e as Int
            l.subList(s, e)
        }
    }
}

val take = { n: Any ->
    { l: Any ->
        n as Int; l as List<Any>
        if (n < 1) {
            emptyList()
        } else {
            l.subList(0, n)
        }
    }
}

val drop = { n: Any ->
    { l: Any ->
        n as Int; l as List<Any>
        if (n < 1) {
            l
        } else {
            l.subList(n, l.size)
        }
    }
}

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

val zipWith = { f: Any ->
    { xs: Any ->
        { ys: Any ->
            f as (Any) -> ((Any) -> Any); xs as List<Any>; ys as List<Any>
            xs.zip(ys).map { (x, y) ->
                f(x)(y)
            }
        }
    }
}

//------------------------------------------------------------------------------
// Partial ---------------------------------------------------------------------
//------------------------------------------------------------------------------

val unsafeIndexImpl = { xs: Any ->
    { n: Any ->
        xs as List<Any>; n as Int
        xs[n]
    }
}