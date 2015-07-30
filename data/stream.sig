
type 'a task = 'a AthenaAsync.Task.task
type 'a seq = 'a Seq.seq

signature STREAM =
sig

    type 'a stream

    val append : 'a stream * 'a stream -> 'a stream task

    val read : 'a stream -> ('a * 'a stream) option task

    val cons : 'a -> 'a stream -> 'a stream task

    val empty : unit -> 'a stream task

    val isEmpty : 'a stream -> bool task

    val toSeq : 'a stream -> 'a seq task

    val fromSeq : 'a seq -> 'a stream task

    val ::: : 'a * ('b -> 'a stream task) -> 'b
              -> 'a stream task

    val singleton : 'a -> 'a stream task

    val map : ('a -> 'b task) -> 'a stream -> 'b stream task
end
