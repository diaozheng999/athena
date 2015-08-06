structure Main =
struct

open Athena
open Athena.Async.Task
open Athena.Async.Timer

infix <| <|| await
infixr |> ||>

val print = async print

fun updateTimer () =
    let val t = Timer.startRealTimer ()
	val init = Time.toSeconds (Timer.checkRealTimer t)
	fun tick i =
	    let val v = Time.toSeconds (Timer.checkRealTimer t)-
			init
	    in
		async IntInf.compare
		      (v,IntInf.fromInt i)
		      await
		      (fn GREATER => yield (IntInf.toInt v)
		      | _ => tick i)
	    end
    in tick end

fun printInt s = print (Int.toString s^"\n")

fun listn s _ = print (s^"\n")

fun main args =
    let val (tmr, run) = start (updateTimer ())
    in c[addContinuousTrigger tmr printInt,
	 addValueTrigger tmr (10, listn "foo"),
	 addValueTrigger tmr (11, listn "bar"),
	 addValueTrigger tmr (20, listn "baz"),
	 addValueTrigger tmr (50, (fn _ => stop tmr)),
	 cdr (setInterval (2000, (print "boo\n"))),
	 addValueTrigger tmr
			 (25, fn _ => print "adding more triggers..\n"
					    ||> addValueTrigger tmr (10, listn "foo")
					    ||> addValueTrigger tmr (25, listn "bar")
					    ||> addValueTrigger tmr (26, listn "foo2")
					    ||> addValueTrigger tmr (26, listn "foo3")
					    ||> addValueTrigger tmr (30, listn "baaa")
					    ||> addContinuousTrigger tmr (listn "===")),
	 run] end

val _ = Program.Bootstrapper.exec main

end
