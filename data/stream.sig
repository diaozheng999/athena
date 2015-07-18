signature STREAM =
sig

    type 'a task
    type 'a stream
    type 'a pair
    type 'a sequence

    val append : 'a stream * 'a stream -> 'a stream task

    val read : 'a stream -> ('a option * 'a stream) task

    val write : ('a stream * 'a) -> 'a stream task

    val flush : 'a stream -> 'a stream task

    val seek : 'a stream * int -> 'a stream task

    val writeSeq : 'a stream * 'a seq -> 'a stream task

    val toSeq : 'a stream -> 'a seq task

    val fromSeq : 'a seq -> 'a stream task

end
