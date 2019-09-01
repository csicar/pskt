package Foreign.Data.Array.ST

val empty = {
    mutableListOf<Any>()
};

val peekImpl = { just: Any ->
    { nothing: Any ->
        { i: Any ->
            { xs: Any ->
                {
                    i as Int; xs as MutableList<Any>; just as (Any) -> Any
                    when (i >= 0 && i < xs.size) {
                        true -> just(xs[i])
                        false -> nothing;
                    }
                };
            };
        };
    };
};

val poke = { i: Any ->
    { a: Any ->
        { xs: Any ->
            {
                i as Int; xs as MutableList<Any>
                val ret = i >= 0 && i < xs.size;
                if (ret) xs[i] = a;
                ret;
            };
        };
    };
};

val popImpl = { just: Any ->
    { nothing: Any ->
        { xs: Any ->
            {
                just as (Any) -> Any; xs as MutableList<Any>
                if (xs.size > 0) {
                    just(xs.removeAt(xs.size - 1))
                } else {
                    nothing;
                }
            };
        };
    };
};

val pushAll = { ls: Any ->
    { xs: Any ->
        {
            ls as List<Any>; xs as MutableList<Any>
            xs.addAll(ls);
            xs.size
        };
    };
};

val shiftImpl = { just: Any ->
    { nothing: Any ->
        { xs: Any ->
            {
                xs as MutableList<Any>; just as (Any) -> Any
                if (xs.size > 0) {
                    just(xs.removeAt(0))
                } else {
                    nothing;
                }
            };
        };
    };
};

val unshiftAll = { ls: Any ->
    { xs: Any ->
        {
            ls as List<Any>; xs as MutableList<Any>
            xs.addAll(0, ls)
            xs.size
        };
    };
};

val splice = { i: Any ->
    { howMany: Any ->
        { bs: Any ->
            { xs: Any ->
                {
                    i as Int; howMany as Int; bs as List<Any>; xs as MutableList<Any>
                    val ret = 0.rangeTo(howMany).map { xs.removeAt(i) }
                    xs.addAll(i, bs)
                    ret
                };
            };
        };
    };
};

val copyImpl = { xs: Any ->
    {
        xs as MutableList<Any>
        xs.toMutableList()
    };
};

val sortByImpl = { comp: Any ->
    { xs: Any ->
        {
            comp as (Any) -> ((Any) -> Int); xs as MutableList<Any>
            xs.sortWith(Comparator { x: Any, y: Any ->
                comp(x)(y)
            })
        };
    };
};

val toAssocArray = { xs: Any ->
    {
      xs as MutableList<Any>
      xs.mapIndexed { index, any -> index to any }.toMap()
    };
};