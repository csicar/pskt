PsKt
====

Kotlin-Backend for PureScript

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
$ git clone ... src/main/kotlin/kotlin-foreigns
# setup purescript repo

$ mkdir purescript
$ cd purescript
$ spago init
$ spago build -- -g corefn
# build kotlin files
$ pskt -o ../src/main/kotlin/output/ -f ../src/main/kotlin/kotlin-foreigns
```

Implementation
--------------

PureScript | Kotlin
---------- | ------
Int | Int
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