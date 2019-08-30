package Test;

import Foreign. PsRuntime.app

val add = {a:Int -> {b:Int -> a+b}}


fun main() {
    val a : Any = 2
    val b : Any = 34
    val m = PS.Main.Module.main as () -> Unit
    println(m())
}