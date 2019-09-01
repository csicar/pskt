package Foreign.Data.FunctorWithIndex

val mapWithIndexArray = { i: Any ->
    { f: Any ->
        {xs: Any ->
            i as Int; f as (Int) -> ((Any) -> Any); xs as List<Any>
            xs.mapIndexed { index, x -> f(i)(x) }
        }
    }
}