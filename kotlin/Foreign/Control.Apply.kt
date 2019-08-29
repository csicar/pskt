package Foreign.Control.Apply;

val arrayApply = { fs: Any ->
  { xs: Any ->
    fs as List<Any>; xs as List<Any>
    xs.zip(fs).map { (f, x) ->
      f as (Any) -> Any
      f(x)
    }
  }
}