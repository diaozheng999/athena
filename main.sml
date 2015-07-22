structure Main =
struct

open Athena
open Athena.Core.Task


fun main args =
    async (fn () => case Int.maxInt of
			SOME i => print ("Max int: "^Int.toString i^"\n")
		      | NONE => print "No max int.\n") ()

val _ = Program.Bootstrapper.exec main

end
