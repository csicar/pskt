package Foreign.Main;

val println = {str: Any -> println(str)}


val unit = Unit

val showIntImpl = fun(a: Int) = "$a"
val showNumberImpl = { a: Number -> "$a"}
val showCharImpl = {a: Char -> "$a"}
val showStringImpl = {a: String -> "$a"}
val showArrayImpl = {f: (Any) -> String -> {a: List<Any> -> "${a.map {f(it)}}" }}
val cons = {elem: Any -> { list: List<Any> -> listOf(elem) + list }}
val join = {a: String -> { list: List<String> -> list.joinToString(a) }}

//foreign import intAdd :: Int -> Int -> Int
//foreign import intMul :: Int -> Int -> Int
//foreign import numAdd :: Number -> Number -> Number
//foreign import numMul :: Number -> Number -> Number

val intAdd = { a: Int -> { b: Int -> a+b }}
val intMul = { a: Int -> { b: Int -> a*b }}
val numAdd = { a: Double -> { b: Double -> a+b }}
val numMul = { a: Double -> { b: Double -> a*b }}

val arrayMap = {f: (Any) -> Any -> {a: List<Any> -> a.map {f(it)} }}