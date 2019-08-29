package Foreign.Effect;

val pureE = { a : Any -> {
  a
}}

val bindE = { a: Any -> { f: Any -> {
  f as (Any) -> Any
  a as () -> Any
  (f(a()) as () -> Any)()
}}}

val untilE = { f: Any -> {
  f as () -> Boolean
  while(f().not());
  Unit;
}}

val whileE = { f: Any -> { a: Any -> {
  f as () -> Boolean
  a as () -> Any
  while(f()) {
    a()
  };
  Unit;
}}}
val forE = { lo : Any -> { hi : Any -> { f : Any -> {
  lo as Int; hi as Int; f as (Int) -> Any

  for (i in lo .. hi) {
    (f(i) as () -> Any)();
  }
  Unit;
}}}}

val foreachE = { ls : Any -> { f : Any -> {
  ls as List<Any>; f as (Any) -> Any
  ls.forEach {
    (f(it) as () -> Any)();
  }
  Unit;
}}}