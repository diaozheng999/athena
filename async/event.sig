type 'a task = 'a Task.task
type serialised = AthenaCore.Serialiser.serialised

signature EVENT =
sig

    type event
    type listener = serialised -> unit task
    type registry
    type serialised

    val mkRegistry : unit -> registry

    val startEventSystem : registry -> unit task

    val addListener : (string -> listener -> int task) ref

    val removeListener : (string -> int -> unit task) ref

    val raiseEvent : (string * serialised -> unit task) ref

end (* end signature EVENT *)
