package Foreign.Data.Int;

import java.lang.Integer.parseInt
import kotlin.math.pow

val fromNumberImpl = { just: Any ->
  { nothing: Any ->
    { n: Any ->
      n as Number; just as (Any) -> Any
      if (n.toInt() == n) {
        just(n)
      } else {
        nothing
      }
    };
  };
};

val toNumber = { n: Any ->
  n as Int
  n.toDouble()
};

val fromStringAsImpl = { just: Any ->
  { nothing: Any ->
    { radix: Any ->
      just as (Any) -> Any; radix as Int
      var digits = "";
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix == 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + (86 + radix).toChar().toString() + "]";
      }
      val pattern = Regex("^[\\+\\-]?$digits+$", RegexOption.IGNORE_CASE);

      { s: Any ->
        /* jshint bitwise: false */
        s as String
        if (pattern.matches(s)) {
          val i = parseInt(s, radix);
          just(i)
          //(i | 0) === i ? just(i) : nothing;
        } else {
          nothing;
        }
      };
    };
  };
};

val toStringAs = { radix: Any ->
  { i: Any ->
    radix as Int; i as Int
    i.toString(radix);
  };
};


val quot = { x: Any ->
  { y: Any ->
    x as Int; y as Int
    x / y
  };
};

val rem = { x: Any ->
  { y: Any ->
    x as Int; y as Int
    x % y;
  };
};

val pow = { x: Any ->
  { y: Any ->
    x as Int; y as Int
    x.toDouble().pow(y).toInt()
  };
};