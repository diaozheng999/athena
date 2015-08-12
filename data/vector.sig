type 'a task = 'a AthenaAsync.Task.task

signature ATHENA_VECTOR =
sig

val fromList : 'a list -> 'a vector task
val tabulate : int * (int -> 'a task) -> 'a vector task
val length : 'a vector -> int task
val sub : 'a vector * int -> 'a task
val update : 'a vector * int * 'a -> 'a vector task
val concat : 'a vector list -> 'a vector task
val appi : (int * 'a -> unit task) -> 'a vector -> unit task
val app : ('a -> unit task) -> 'a vector -> unit task
val mapi : (int * 'a -> 'b task) -> 'a vector -> 'b vector task
val map : ('a -> 'b task) -> 'a vector -> 'b vector task
val foldli : (int * 'a * 'b -> 'b task) -> 'b -> 'a vector -> 'b task
val foldri : (int * 'a * 'b -> 'b task) -> 'b -> 'a vector -> 'b task
val foldl : ('a * 'b -> 'b task) -> 'b -> 'a vector -> 'b task
val foldr : ('a * 'b -> 'b task) -> 'b -> 'a vector -> 'b task
val findi : (int * 'a -> bool task) -> 'a vector -> (int * 'a) option task
val find : ('a -> bool task) -> 'a vector -> 'a option task
val exists : ('a -> bool task) -> 'a vector -> bool task
val all : ('a -> bool task) -> 'a vector -> bool task
val collate : ('a * 'a -> order task) -> 'a vector * 'a vector -> order task

end
