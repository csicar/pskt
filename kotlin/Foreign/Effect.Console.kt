package Foreign.Effect.Console

val log = { s: Any ->
  {
    println(s)
  }
};

val warn = { s: Any ->
  {
    println(s)
  }
};

val error = { s: Any ->
  {
    println(s)
  }
};

val info = { s: Any ->
  {
    println(s)
  }
};

val stopWatches = mutableMapOf<String, Long>()

val time = { s: Any ->
  {
    s as String
    stopWatches[s] = System.currentTimeMillis()
  }
};

val timeEnd = { s: Any ->
  {
    val end = System.currentTimeMillis()
    val startTime = stopWatches[s]
    if (startTime == null) {
      println("Timed $s: timer not started")
    } else {
      println("Timed $s : ${end - startTime}")
    }
  }
};