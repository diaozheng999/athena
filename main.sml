structure Main =
struct

open Athena
open Athena.Core
open Athena.Data
open Athena.Core.Task
infix ||> |>


val reg = Event.mkRegistry ()



fun main args = 
    async (fn () => case Int.maxInt of
			SOME i => print ("Max int: "^Int.toString i^"\n")
		      | NONE => print "No max int.\n") ()      
			   


fun bootstrapper () = ignore (!Event.addListener "__start" main) 
			     ||> (!Event.raiseEvent ("__start",Serialiser.packUnit ()))

val prog = Event.startEventSystem |> bootstrapper

val () = run (prog reg)

end
