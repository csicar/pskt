package Foreign.Data.Functor

val arrayMap = { f: Any ->
  { arr: Any ->
    f as (Any) -> Any; arr as List<Any>
    arr.map(f)
  }
}