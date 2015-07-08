signature STREAM =
sig

    type 'a task
    type 'a stream
    type 'a pair
    type 'a sequence

    val append : 'a stream * 'a stream -> 'a stream task

    val combine : ('a * 'a -> 'a task) -> 'a pair -> 'a task

    val combine2 : ('a * 'a -> 'a task) -> ('a -> 'a task) -> 'a pair -> 'a task

    val cons : 'a -> 'a stream -> 'a stream task

    val drop : int -> 'a stream -> 'a stream task

    val empty : unit -> 'a stream task

    val exists : ('a -> bool task) -> 'a stream -> bool task

    val filter : ('a -> bool task) -> 'a stream -> 'a stream task

    val flatten : 'a stream stream -> 'a stream task

    val forall : ('a -> bool task) -> 'a stream -> bool task

    val length : 'a stream -> int option task

    val map : ('a -> 'b task) -> 'a stream -> 'b stream task

    val mapreduce : ('a -> 'b task) -> 'b -> ('b * 'b -> 'b task) -> 'a stream -> 'b task

    val mapreduce1: ('a -> 'b task) -> ('b * 'b -> 'b task) -> 'a stream -> 'b task

    val null : 'a stream -> bool task

    val nth : int -> 'a stream -> 'a task

    val pairwise : 'a stream -> 'a pair stream 'a task

    val reduce : ('a * 'a -> 'a task) -> 'a -> 'a stream -> 'a task

    val reduce1 : ('a * 'a -> 'a task) -> 'a stream -> 'a task

    val reduce2 : ('a * 'a -> 'a task) -> ('a -> 'a task) -> 'a stream -> 'a task

    val repeat : int option -> 'a -> 'a stream task

    val singleton : 'a -> 'a stream task

    val split : int -> 'a stream -> ('a stream * 'a stream) task

    val tabulate : (int -> 'a task) -> int -> 'a stream task

    val take : int -> 'a stream -> 'a stream task

    val toString : ('a -> string task) -> 'a stream -> string task

    val zip : 'a stream * 'b stream -> ('a * 'b) stream task

    val toSeq : 'a stream -> 'a seq task

    val fromSeq : 'a seq -> 'a stream task

end
