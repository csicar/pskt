PsKt
====

Kotlin-Backend for PureScript

Why
---

The goal of this project is to support native android development in PureScript using Kotlin. Kotlin was chosen, because it has first-class support from Android. This means, that the complete API is available through the FFI.

Installation
------------

```bash
$ git clone ....
$ make
$ make install
# pskt should no be available in your PATH
$ pskt
```

Project Setup
-------------

```bash
$ cd your-project
# import foreign files for kotlin
$ git clone https://github.com/csicar/pskt-foreigns

# setup purescript
$ mkdir purescript
$ cd purescript
$ spago init
$ spago build -- -g corefn
# build kotlin files
$ pskt -o ../src/main/kotlin/output/ -f "../pskt-foreigns/*.kt"
# transpiled files will be written to ../src/main/kotlin/output/
```

### Troubleshooting

When compiling with `kotlinc` you might get some weird errors like `OutOfMemory` exceptions. For some reason compiling in Intellij or Android Studio will solve that problem.

Implementation
--------------

PureScript | Kotlin
---------- | ------
Int | Int (might be changed to Long)
Number | Double
Array | List<Any>
String | String
Char | Char
Boolean | Boolean
Records | Map<String, Any>
Unit | Unit

Data types are faithfully represented using `sealed classes`:

```purescript
data T a = T a | B Int String
```

becomes

```kotlin
sealed class _Type_T() {
  data class T(val value0: Any)
  data class B(val value0: Any, val value1: Any)
}
```

Function calls are sugared with `.app(...)` to make the generated code more readable. `app` is an extension function, that can be inlined by the kotlin compiler adding to runtime overhead.

### Modules

- All transpiled Purescript modules will be located in the `PS.` package in Kotlin (eg. `Data.Array` in PS will be `PS.Data.Array` in Kt)
- Definitions in a module in Purescript will be located in the `Module` object (eg. `Data.Array.range` in PS will be `PS.Data.Array.Module.range` in Kt)
- Foreign definitions are located in the `Foreign.` package in Kotlin, followed by the same Namespace that is used in Purescript (eg. the foreign definition `arrayMapImpl` in the module `Data.Array` should be located in `Foreign.Data.Array` in Kotlin)


FFI
---

Implementations for FFI files for prelude and some default libraries can be found [here](https://github.com/csicar/pskt-foreigns). The implementation is not complete but mostly working.

FFIs should be implemented using lambda expressions (not anonymous functions) and accept `Any` as the arguments:

```kotlin
package Foreign.Data.MyFFI;

val myFFI = { str: Any ->
  { f: Any ->
    str as String; f as (String -> (Int -> Double))

    f(str)(2)
  }
}
```

The actual types can be added using casts in the body. Kotlin will smart-cast the arguments (here `str` and `f`) to their respective type.

`Effect a` is represented as `() -> Any`; just like PureScript represents it in JavaScript.

Note, that names are normalized: `_` is replaced by `__` and `$` is replaced by `_dollar`. For example if you define a foreign import named `_myFFI`  in purescript, the corresponding definition in kotlin must be named `__myFFI`.

Android
-------

An example for android can be found [here](https://github.com/csicar/pskt-android-example).
