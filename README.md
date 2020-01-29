PsKt
====

Kotlin-Backend for PureScript. PsKt is forked from [purescript-native](https://github.com/andyarvanitis/purescript-native)

[![pipeline status](https://gitlab.com/csicar/pskt/badges/kotlin/pipeline.svg)](https://gitlab.com/csicar/pskt/commits/kotlin)

Why
---

The goal of this project is to support native android development in PureScript using Kotlin. Kotlin was chosen, because it has first-class support from Andsroid. This means, that the complete API is available through the FFI.

Installation
------------

There are some pre-build executables available:
  - [Archlinux](https://gitlab.com/csicar/pskt/-/jobs/artifacts/kotlin/download?job=build)
  - [Ubuntu](https://gitlab.com/csicar/pskt/-/jobs/artifacts/kotlin/download?job=build-ubuntu)

To build the executable:

```bash
$ git clone https://github.com/csicar/pskt
$ make
$ make install
# pskt should no be available in your PATH
$ pskt
```

Project Setup
-------------

Spago now has buildin support for alternative backends. 
To build you project for kotlin:

```bash
$ cd your-project
$ spago init
# tell spago that you want to use the pskt backend
$ sed -i '6 a , backend = \n    "pskt"' spago.dhall
# now use spago normally
$ spago build
# compiled files can be found in output/pskt
$ ls output/pskt
Main.kt Prelude.kt ...
```

To run build a jar file and run it, you will first need to import foreign-files:
```bash
$ git clone git@github.com:csicar/pskt-foreigns.git
$ spago run --node-args --foreigns --node-args ./pskt-foreigns
```

### Troubleshooting

When compiling with `kotlinc` you might get some weird errors like `OutOfMemory` exceptions. 
This can be fixed by using a gradle build script. See [kotlin test](https://github.com/csicar/pskt/tree/kotlin/kotlin/build.gradle) for an example. 

Sometimes the kotlin compiler reports this error: `e: ...Main.kt: (6, 8): Redeclaration: Module`
The fix is removing the gradle build folder and compiling again: `rm -r build`

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

### Bootstrapping

When `pskt` is started with the `--run` option, bootstrapping will be done by the compiler. In that case, the entry point of the purescript program is expected to be `Main.main` with type `:: Effect _`.

It is also possible to do custom bootstrapping: 

To run a purescript program `main :: Effect Unit` defined in the module `MyModule`:
```kotlin
fun main() {
  PS.MyModule.Module.main() // or use .appRun() if the type of `main` is Any
}
```

If your `main` function expects additional arguments, you call it like this: `PS.MyModule.Module.main.app(activity).appRun()`


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


ToDos
-----

- [x] MagicDo
- [x] Build cache
 - [x] use Shake
- [x] `--run` for spago CLI spec
- [x] TCO: through Kotlin's `tailrec`
- [ ] expand inlining
  - [x] `$`
  - [ ] `>`, `>=` ...
- [ ] Android library
  - beginnings in the [android example](https://github.com/csicar/pskt-android-example/tree/master/app/src/main/java/de/csicar/myapplication)
- [ ] Aff 
  - build on Coroutines' `Async` & `Deferred` ?
    - `bind` : `{a, f -> async { f(a.await()) }`
    - ...
  - reuse message queue? https://developer.android.com/reference/android/os/MessageQueue
