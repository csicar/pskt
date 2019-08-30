package Foreign.Effect.Ref;

class Ref(var value: Any)

val new = { v: Any ->
    {
        Ref(v)
    };
};

val read = { ref: Any ->
    {
        ref as Ref
        ref.value;
    };
};

val modify_tick = { f: Any ->
    { ref: Any ->
        {
            f as (Any) -> Any; ref as Ref
            val t = f(ref.value) as Map<String, Any>
            ref.value = t["state"]!!;
            t["value"]!!;
        };
    };
};

val write = { v: Any ->
    { ref: Any ->
        {
            ref as Ref
            ref.value = v;
            Unit
        };
    };
};