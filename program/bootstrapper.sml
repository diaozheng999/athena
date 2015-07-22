structure Bootstrapper :> BOOTSTRAPPER
                              where type 'a task = 'a AthenaCore.Task.task
 =
struct

open AthenaCore
open AthenaData
open AthenaCore.Task
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