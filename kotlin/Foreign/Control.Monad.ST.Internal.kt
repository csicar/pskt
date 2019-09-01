package Foreign.Control.Monad.ST.Internal;


val map_ = { f: Any ->
  { a: Any ->
    {
      f as (Any) -> Any; a as () -> Any
      f(a());
    };
  };
};

val pure_ = { a: Any ->
  {
    a;
  };
};

val bind_ = { a: Any ->
  { f: Any ->
    {
      f as (Any) -> (() -> Any); a as () -> Any
      f(a())();
    };
  };
};

val run = { f: Any ->
  f as () -> Any
  f();
};

val _while = { f: Any ->
  { a: Any ->
    {
      f as () -> Boolean; a as () -> Any
      while (f()) {
        a();
      }
    };
  };
};

val _for = { lo: Any ->
  { hi: Any ->
    { f: Any ->
      {
        lo as Int; hi as Int; f as (Int) -> (() -> Any)
        (lo .. hi).forEach {
          f(it)()
        }
      };
    };
  };
};

val foreach = { ls: Any ->
  { f: Any ->
    {
      ls as List<Any>; f as (Any) -> (() -> Any)
      ls.forEach {
        f(it)()
      }
    };
  };
};

data class StValue(var value: Any)

val new = { v: Any ->
  {
    StValue(v)
  };
};

val read = { ref: Any ->
  {
    ref as StValue
    ref.value;
  };
};

val modify_tick = { f: Any ->
  { ref: Any ->
    {
      ref as StValue; f as (Any) -> Map<String, Any>
      val t = f(ref.value);
      ref.value = t["state"]!!
      t["value"]!!
    };
  };
};

val write = { a: Any ->
  { ref: Any ->
    {
      ref as StValue;
      ref.value = a;
      a
    };
  };
};