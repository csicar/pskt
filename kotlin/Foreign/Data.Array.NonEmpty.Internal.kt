package Foreign.Data.Array.NonEmpty.Internal

val fold1Impl = { f: Any ->
    { ls: Any ->
        ls as List<Any>; f as (Any) -> ((Any) -> Any)
        var acc = ls[0]
        ls.forEach {
            acc = f(acc)(it)
        }
        acc
    }
}
data class Cont(val fn: () -> Any)
data class ConsCell(val head: Any, val tail: Any)
val traverse1Impl = {
    val emptyList = Unit
    val finalCell = {head: Any -> ConsCell(head, emptyList)}
    val consList = {x: Any -> {xs: Any -> ConsCell(x, xs)}}
    fun listToArray(list: ConsCell): List<Any> {
        var arr = mutableListOf<Any>()
        var xs = list
        while (true) {
            arr.add(xs.head)
            val tail = xs.tail
            if (tail is ConsCell) {
                xs = tail
            } else {
                return arr
            }
        }
    }
    { apply: Any ->
        { map: Any ->
            { f: Any ->
                apply as (Any) -> ((Any) -> ((Any) -> Any))
                map as (Any) -> ((Any) -> ((Any) -> Any))
                f as (Any) -> Any
                val buildFrom = { x: Any, ys: Any ->
                    apply(map(consList)(f(x)))(ys)
                }
                fun go(acc: Any, currentLen: Int, xs: List<Any>) {
                    when (currentLen) {
                        0 -> acc
                        else -> {
                            val last = xs.last()
                            Cont {
                                go(buildFrom(last, acc), currentLen - 1, xs)
                            }
                        }
                    }
                }
                { array: Any ->
                    array as List<Any>
                    val acc = map(finalCell)(f(array[array.size - 1]))
                    var result : Any = go(acc, array.size -1, array)
                    while (result is Cont) {
                        result = result.fn()
                    }
                    map(::listToArray)(result)
                }
            }
        }
    }
}