package Foreign.Control.Bind;

val arrayBind = { arr: Any ->
  { f: Any ->
    arr as List<Any>; f as (Any) -> List<Any>
    arr.flatMap(f)
  }
}
