package Foreign.Data.Traversable;

import kotlin.math.floor

val traverseArrayImpl = {
    val list0 = emptyList<Any>();
    val list1 = { a: Any -> listOf(a) }
    val list2 = { a: Any -> { b: Any -> listOf(a, b) } }
    val list3 = { a: Any -> { b: Any -> { c: Any -> listOf(a, b, c) } } }
    val concat2 = { a: List<Any> -> { b: List<Any> -> a + b } }

    { apply: Any ->
        { map: Any ->
            { pure: Any ->
                { f: Any ->
                    { array: Any ->
                        pure as (Any) -> Any; array as List<Any>; f as (Any) -> Any; map as (Any) -> ((Any) -> Any); apply as (Any) -> ((Any) -> Any)
                        fun go(bot: Int, top: Int) {
                            when (bot - top) {
                                0 -> pure(list0)
                                1 -> map(list1)(f(array[bot]))
                                2 -> apply(map(list2)(f(array[bot])))(f(array[bot + 1]))
                                3 -> apply(apply(map(list3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]))
                                else -> {
                                    val pivot = bot + floor((top.toDouble() - bot.toDouble()) / 4) * 2;
                                    apply(map(concat2)(go(bot, pivot.toInt())))(go(pivot.toInt(), top))
                                }
                            }
                        }
                        go(0, array.size)
                    }
                }
            }
        }
    }
}()