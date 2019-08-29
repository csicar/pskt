package Foreign.Effect.Unsafe;

val unsafePerformEffect = { f: Any -> 
  f as () -> Any
  f()
}