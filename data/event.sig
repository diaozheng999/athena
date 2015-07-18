signature EVENT =
sig
    structure T :
              sig
                type 'a task
              end

    type event
    type listener
    type registry
    type serialised

    val mkRegistry : unit -> registry

    val startEventSystem : registry -> unit T.task

    val addListener : (string -> listener -> int T.task) ref

    val removeListener : (string -> int -> unit T.task) ref

    val raiseEvent : (string * serialised -> unit T.task) ref

end
