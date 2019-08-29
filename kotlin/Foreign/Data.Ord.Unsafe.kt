package Foreign.Data.Ord.Unsafe;

val unsafeCompareImpl = { lt: Any ->
    { eq: Any ->
        { gt: Any ->
            {x:Any ->
                {y:Any->
                    error("unsafeCompare not available in pskt")
                }
            }
        }
    }
}