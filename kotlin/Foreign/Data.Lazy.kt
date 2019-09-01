package Foreign.Data.Lazy

val defer = { thunk: Any? ->
  var v : Any? = null;
  {
    // Simple Impl: might not help the gc as much as the JS impl, but
    // assigning param is not allowed in kt
    if (v != null) {
      v
    } else {
      thunk as () -> Any
      v = thunk()
      v
    }
//    if (thunk == null){
//      v
//    } else {
//      thunk as () -> Any
//      v = thunk();
//      thunk = null; // eslint-disable-line no-param-reassign
//      v;
//    }

  };
};

val force = {l: Any ->
  l as () -> Any
  l();
};