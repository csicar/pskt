@file:Suppress("UNCHECKED_CAST")

package Foreign.PsRuntime;

inline fun Any.app(arg: Any): Any {
    return (this as (Any) -> Any)(arg)
}