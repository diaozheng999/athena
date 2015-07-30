
type 'a task = 'a AthenaAsync.Task.task

signature BOOTSTRAPPER =
sig
  val exec : (unit -> unit task) -> unit
end
