package Foreign.Partial;

val crashWith = {
  { msg: String ->
    error(msg)
  }
}