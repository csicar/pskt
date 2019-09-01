package Foreign.Data.Array.ST.Partial

val peekImpl ={ i: Any ->
  { xs: Any ->
    {
      xs as Map<String, Any>; i as String
      xs[i];
    };
  };
};

val pokeImpl ={ i: Any ->
  { a: Any ->
    { xs: Any ->
      {
        xs as MutableMap<String, Any>; i as String
        xs[i] = a;
        Unit
      };
    };
  };
};