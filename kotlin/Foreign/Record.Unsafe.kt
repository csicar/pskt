package Foreign.Record.Unsafe;

val unsafeHas = fun(label: Any) = fun (rec: Any) = (rec as Map<Any, Any>)[label] != null

val unsafeGet = fun(label: Any) = fun (rec: Any) = (rec as Map<Any, Any>)[label]!!

val unsafeSet = { label : Any -> { value: Any -> { rec: Any ->
  label as String
  rec as Map<Any, Any>
  val copy = rec.toMutableMap()
  copy[label] = value
  copy.toMap()
}}}

val unsafeDelete = { label: Any -> { rec: Any -> 
  label as String
  rec as Map<Any, Any>
  rec.filterKeys { it != label }
}}