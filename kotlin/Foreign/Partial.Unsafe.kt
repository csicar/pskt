package Foreign.Partial.Unsafe;

val unsafePartial = {
  { f: Any ->
    f as () -> Any
    f()
  }
}