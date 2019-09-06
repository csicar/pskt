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


FFI
---


FFIs should be implemented using lambda expressions (not anonymous functions) and accept `Any` as the arguments:

```kotlin
val myFFI = { str: Any ->
  { f: Any ->
    str as String; f as (String -> (Int -> Double))

    f(str)(2)
  }
}
```

The actual types can be added using casts in the body. Kotlin will smart-cast the arguments (here `str` and `f`) to their respective type.

`Effect a` is represented as `() -> Any`; just like PureScript represents it in JavaScript.

Android
-------

An example for android can be found [here](https://github.com/csicar/pskt-android-example).