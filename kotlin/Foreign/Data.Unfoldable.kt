package Foreign.Data.Unfoldable;

val unfoldrArrayImpl = { isNothing: Any ->
    { fromJust: Any ->
        { fst: Any ->
            { snd: Any ->
                { f: Any ->
                    { b: Any ->
                        f as (Any) -> Any; isNothing as (Any) -> Boolean; fromJust as (Any) -> Any; fst as (Any) -> Any; snd as (Any) -> Any

                        var value = b;

                        val result = mutableListOf<Any>();

                        while (true) { // eslint-disable-line no-constant-condition
                            var maybe = f(value);
                            if (isNothing(maybe)) {
                                break;
                            };
                            var tuple = fromJust(maybe);
                            result.add(fst(tuple));
                            value = snd(tuple);
                        }
                        result.toList()
                    }
                };
            };
        };
    };
};