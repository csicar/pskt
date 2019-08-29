package Foreign.Effect.Uncurried;

val mkEffectFn1 = { fn: Any ->
    { x: Any ->
        fn as (Any) -> (() -> Any)
        fn(x)();
    }
}

val mkEffectFn2 = { fn: Any ->
    { a: Any, b: Any ->
        fn as (Any) -> ((Any) -> (() -> Any))
        fn(a)(b)();
    }
}

val mkEffectFn3 = { fn: Any ->
    { a: Any, b: Any, c: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> (() -> Any)))
        fn(a)(b)(c)();
    }
}

val mkEffectFn4 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any))))
        fn(a)(b)(c)(d)();
    }
}

val mkEffectFn5 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any, e: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any)))))
        fn(a)(b)(c)(d)(e)();
    }
}

val mkEffectFn6 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any, e: Any, f: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any))))))
        fn(a)(b)(c)(d)(e)(f)();
    }
}

val mkEffectFn7 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any)))))))
        fn(a)(b)(c)(d)(e)(f)(g)();
    }
}

val mkEffectFn8 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any))))))))
        fn(a)(b)(c)(d)(e)(f)(g)(h)();
    }
}

val mkEffectFn9 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any)))))))))
        fn(a)(b)(c)(d)(e)(f)(g)(h)(i)();
    }
}

val mkEffectFn10 = { fn: Any ->
    { a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any, i: Any, j: Any ->
        fn as (Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> ((Any) -> (() -> Any))))))))))
        fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)();
    }
}

val runEffectFn1 = { fn: Any ->
    { a: Any ->
        {
            fn as (Any) -> Any
            fn(a);
        }
    }
}

val runEffectFn2 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            {
                fn as (Any, Any) -> Any
                fn(a, b)
            }
        }
    }
}

val runEffectFn3 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                {
                    fn as (Any, Any, Any) -> Any
                    fn(a, b, c)
                }
            }
        }
    }
}

val runEffectFn4 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    {
                        fn as (Any, Any, Any, Any) -> Any
                        fn(a, b, c, d)
                    }
                }
            }
        }
    }
}

val runEffectFn5 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    { e: Any ->
                        {
                            fn as (Any, Any, Any, Any, Any) -> Any
                            fn(a, b, c, d, e)
                        }
                    }
                }
            }
        }
    }
}

val runEffectFn6 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    { e: Any ->
                        { f: Any ->
                            {
                                fn as (Any, Any, Any, Any, Any, Any) -> Any
                                fn(a, b, c, d, e, f)
                            }
                        }
                    }
                }
            }
        }
    }
}

val runEffectFn7 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    { e: Any ->
                        { f: Any ->
                            { g: Any ->
                                {
                                    fn as (Any, Any, Any, Any, Any, Any, Any) -> Any
                                    fn(a, b, c, d, e, f, g)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

val runEffectFn8 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    { e: Any ->
                        { f: Any ->
                            { g: Any ->
                                { h: Any ->
                                    {
                                        fn as (Any, Any, Any, Any, Any, Any, Any, Any) -> Any
                                        fn(a, b, c, d, e, f, g, h)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

val runEffectFn9 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    { e: Any ->
                        { f: Any ->
                            { g: Any ->
                                { h: Any ->
                                    { i: Any ->
                                        {
                                            fn as (Any, Any, Any, Any, Any, Any, Any, Any, Any) -> Any
                                            fn(a, b, c, d, e, f, g, h, i)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

val runEffectFn10 = { fn: Any ->
    { a: Any ->
        { b: Any ->
            { c: Any ->
                { d: Any ->
                    { e: Any ->
                        { f: Any ->
                            { g: Any ->
                                { h: Any ->
                                    { i: Any ->
                                        { j: Any ->
                                            {
                                                fn as (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) -> Any
                                                fn(a, b, c, d, e, f, g, h, i, j)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}