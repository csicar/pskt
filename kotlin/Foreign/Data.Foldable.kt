package Foreign.Data.Foldable;

val foldrArray = {f: Any ->
  { init: Any ->
    { xs: Any ->
      f as (Any) -> ((Any) -> Any); xs as List<Any>
      var acc = init
      xs.reversed().forEachIndexed { index, x ->
        acc = f(x)(acc)
      }
      acc;
    };
  };
};

val foldlArray = {f: Any ->
  { init: Any ->
    { xs: Any ->
      f as (Any) -> ((Any) -> Any); xs as List<Any>
      var acc = init
      xs.forEachIndexed { index, x ->
        acc = f(x)(acc)
      }
      acc;
    };
  };
};
