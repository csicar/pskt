package Foreign.Data.Unfoldable1;

val unfoldr1ArrayImpl = { isNothing: Any ->
    { fromJust: Any ->
        { fst: Any ->
            { snd: Any ->
                { f: Any ->
                    { b: Any ->
                        f as (Any) -> Any; isNothing as (Any) -> Boolean; fromJust as (Any) -> Any; fst as (Any) -> Any; snd as (Any) -> Any

                        var value = b;

                        val result = mutableListOf<Any>();

                        while (true) { // eslint-disable-line no-constant-condition
                            var tuple = f(value);
                            result.add(fst(tuple));
                            val maybe = snd(tuple);
                            if (isNothing(maybe)) {
                                break;
                            };
                            value = fromJust(maybe)
                        }
                        result.toList()
                    }
                };
            };
        };
    };
};