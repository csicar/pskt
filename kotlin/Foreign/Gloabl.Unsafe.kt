package Foreign.Global.Unsafe;

import java.net.URLDecoder
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

val unsafeStringify = { x: Any ->
    TODO("implement for popular base-types")
    //JSON.stringify(x);
};

val unsafeToFixed = { digits: Any ->
    { n: Any ->
        digits as Int; n as Double
        "%.${digits}f".format(n)
    };
};

val unsafeToExponential = { digits: Any ->
    { n: Any ->
        digits as Int; n as Double
        TODO("implement")
    };
};

val unsafeToPrecision = { digits: Any ->
    { n: Any ->
        digits as Int; n as Double
        "%.${digits}f".format(n)
    };
};

val unsafeDecodeURI = { s: Any -> s as String; URLDecoder.decode(s, StandardCharsets.UTF_8.name()) }
val unsafeEncodeURI = { s: Any -> s as String; URLEncoder.encode(s, StandardCharsets.UTF_8.name()) };
val unsafeDecodeURIComponent = { s: Any -> s as String; URLDecoder.decode(s, StandardCharsets.UTF_8.name()) } //TODO: check is interchangeable
val unsafeEncodeURIComponent = { s: Any -> s as String; URLEncoder.encode(s, StandardCharsets.UTF_8.name()) };