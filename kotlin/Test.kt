package Test;

import Foreign. PsRuntime.app

val add = {a:Int -> {b:Int -> a+b}}

fun main(v: Array<String>) {
    val a : Any = 2
    val b : Any = 34
    println(add.app(a).app(b))
    //println((Main.Module.main as (Any) -> Any)(2))
}