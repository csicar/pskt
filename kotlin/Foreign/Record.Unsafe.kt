package Foreign.Record.Unsafe;

val unsafeHas =  { label: Any -> { rec: Any -> (rec as Map<Any, Any>).containsKey(label) }}

val unsafeGet = {label: Any -> { rec: Any -> (rec as Map<Any, Any>)[label]!! }}

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