package Foreign.Data.Show;

val showIntImpl = { n : Any -> "$n" }

val showNumberImpl = { n : Any -> "$n" }

val showCharImpl = { c : Any -> "$c" }

val showStringImpl = { s : Any -> "$s" }

val showArrayImpl = { f : Any -> { xs : Any ->
  xs as List<Any>
  f as (Any) -> Any
  val ss = xs.map { f(it) }.joinToString(",")
  "[$ss]"
}}

val cons = { head : Any -> { tail : Any ->
  tail as List<Any>
  listOf(head) + tail
}}

val join = { sep: Any -> { xs: Any ->
  xs as List<Any>
  sep as String
  xs.joinToString(sep)
}}