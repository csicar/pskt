package Foreign.Control.Extend;

val arrayExtend = {f: Any ->
  {xs: Any ->
    xs as List<Any>; f as (Any)-> Any
    xs.mapIndexed { i, any ->
      f(xs.subList(i,xs.size))
    }
  };
};