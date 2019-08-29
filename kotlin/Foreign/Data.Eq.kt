package Foreign.Data.Eq;

val refEq = { r1 : Any -> { r2 : Any -> r1 == r2 }}

val eqBooleanImpl = refEq;
val eqIntImpl = refEq;
val eqNumberImpl = refEq;
val eqCharImpl = refEq;
val eqStringImpl = refEq;
val eqArrayImpl = refEq;