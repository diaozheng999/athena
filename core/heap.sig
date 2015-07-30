signature HEAP =
sig

    type 'a elem
    type 'a heap

    val empty : unit -> 'a heap

    val isEmpty : 'a heap -> bool

    val push : 'a heap -> int * 'a -> 'a heap

    val pop : 'a heap -> 'a option * 'a heap

    val peek : 'a heap -> 'a option

    val clone : 'a heap -> 'a heap

    val toList : 'a heap -> 'a option list

    val toArray : 'a heap -> 'a option array

    val toVector : 'a heap -> 'a option vector

end
