package Foreign.Data.Int.Bits;

import java.lang.Integer.parseInt
import kotlin.math.pow


val and = { n1: Any ->
  { n2: Any ->
    n1 as Int; n2 as Int
    n1 and n2;
  };
};

val or = { n1: Any ->
  { n2: Any ->
    n1 as Int; n2 as Int
    n1 or n2;
  };
};

val xor = { n1: Any ->
  { n2: Any ->
    n1 as Int; n2 as Int
    n1 xor n2;
  };
};

val shl = { n1: Any ->
  { n2: Any ->
    n1 as Int; n2 as Int
    n1 shl n2;
  };
};

val shr = { n1: Any ->
  { n2: Any ->
    n1 as Int; n2 as Int
    n1 shr n2;
  };
};

val zshr = { n1: Any ->
  { n2: Any ->
    n1 as Int; n2 as Int
    n1 ushr  n2;
  };
};

val complement = { n: Any ->
  n as Int
  n.inv()
};
