structure Bootstrapper :> BOOTSTRAPPER =
struct

open AthenaCore
open AthenaAsync
open AthenaAsync.Task
infix ||> |>

type 'a task = 'a Task.task



fun bootstrapper f () = ignore (!Event.addListener "__start"
                                                   (fn _ => f ()))
                               ||> (!Event.raiseEvent ("__start",
                                                       Serialiser.packUnit ()))

fun exec f =
    let
      val reg = Event.mkRegistry ()
    in run ((Event.startEventSystem |> bootstrapper f) reg) end

end
