signature BOOTSTRAPPER =
sig

  type 'a task

  val exec : (unit -> unit task) -> unit

end
