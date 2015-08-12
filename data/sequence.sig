
type 'a task = 'a AthenaAsync.Task.task

signature SEQUENCE =
sig

    type 'a seq

    datatype 'a pair = Pair of 'a * 'a | Singleton of 'a

    (* append <x0,...,x{n-1}>, <y0,...,y{m-1}> ==
                                       <x0,...,x{n-1},y0,...,y{m-1} *)
    val append : ('a seq * 'a seq) -> 'a seq task

    (* combines a pair into a single unit, leaving singletons untouched. *)
    val combine : ('a * 'a -> 'a task) -> 'a pair -> 'a task

    (* combine2 comb pass combines a pair into a single unit with comb, if it
       encounters a singleton, it will run pass on it *)
    val combine2 : ('a * 'a -> 'a task)
                   -> ('a -> 'a task)
                   -> 'a pair -> 'a task

    (* cons x <y0,...,yn> returns <x,y0,...,yn> *)
    val cons : 'a -> 'a seq -> 'a seq task

    (* drop returns a sequence that drops the first k elements *)
    val drop : int -> 'a seq -> 'a seq task

    (* returns a representation of the empty sequence.
       where  run ((empty |> null) ()) == true *)
    val empty : unit -> 'a seq task

    (* returns true if there exists an element in the sequence that satisfies
       the predicate *)
    val exists : ('a -> bool task) -> 'a seq -> bool task

    (* filter predicate seq returns a sequence containing all elements that
       satisfies the given predicate *)
    val filter : ('a -> bool task) -> 'a seq -> 'a seq task

    (* flattens a sequence of sequences into a sequence *)
    val flatten : 'a seq seq -> 'a seq task

    (* returns true if all elements in the sequence satisfies the predicate *)
    val forall : ('a -> bool task) -> 'a seq -> bool task

    (* returns true if seq is empty, false otherwise *)
    val isEmpty : 'a seq -> bool task

    (* returns the length of the sequence *)
    val length : 'a seq -> int task

    (* maps the mapper unto all elements of the sequence.
       REQUIRES : mapper is thread-safe and total *)
    val map : ('a -> 'b task) -> 'a seq -> 'b seq task

    (* maps the mapper and then reduces the reduction with the given identity.
       usage: mapreduce mapper identity reduction sequence
       REQUIRES : both mapper and reduction are thread-safe and total *)
    val mapreduce : ('a -> 'b task)
                    -> 'b
                    -> ('b * 'b -> 'b task)
                    -> 'a seq -> 'b task

    (* maps the mapper and then reduces the reduction
       usage: mapreduce1 mapper reduction sequence
       REQUIRES : both mapper and reduction are thread-safe and total,
                  as well as sequence being non-empty *)
    val mapreduce1: ('a -> 'b task)
                    -> ('b * 'b -> 'b task)
                    -> 'a seq -> 'b task

    (* returns if the sequence is empty *)
    val null : 'a seq -> bool task

    (* returns the nth element of the sequence.
       usage: nth i sequence *)
    val nth : int -> 'a seq -> 'a task

    (* creates a sequence of pairs from a sequence. Mainly used for concurrent
        reductions. *)
    val pairwise : 'a seq -> 'a pair seq task

    (* reduction functions. *)
    (* reduce takes a predicate and identity satisfying p (i,x) = p (x,i) = x
       reduce1 takes a predicate only, and assumes the sequence is non-empty
       reduce2 takes a predicate as well as a transformation function that will
                be applied to singletons *)
    val reduce : ('a * 'a -> 'a task) -> 'a -> 'a seq -> 'a task
    val reduce1 : ('a * 'a -> 'a task) -> 'a seq -> 'a task
    val reduce2 : ('a * 'a -> 'a task)
                  -> ('a -> 'a task)
                  -> 'a seq -> 'a task

    (* repeat n value creates a sequence of length n filled with value. *)
    val repeat : int -> 'a -> 'a seq task

    (* creates a singleton sequence *)
    val singleton : 'a -> 'a seq task

    (* splits a sequence up at index k, so the first part
       contains <x0,...,x{k-1}> and
       the second part contains <xk,...,x{n-1}>.
       REQUIRES : 0 <= k < run (length n) *)
    val split : int -> 'a seq -> ('a seq * 'a seq) task

    (* tabulate elttask len creates a sequence of length len with each element
       returned by elttask.
       REQUIRES : elttask is thread-safe *)
    val tabulate : (int -> 'a task) -> int -> 'a seq task

    (* the opposite of drop, returns the first k elements (ids 0...k-1) of
       the sequence *)
    val take : int -> 'a seq -> 'a seq task

    (* given a string representation of the element, returns a string
       representation of the sequence.
       REQUIRES : stringify is thread-safe *)
    val toString : ('a -> string task) -> 'a seq -> string task

    (* zip takes two sequences and combines them into one, and truncates the
       tail of the longer sequence *)
    val zip : 'a seq * 'b seq -> ('a * 'b) seq task


    val fromList : 'a list -> 'a seq task
    val fromVector : 'a vector -> 'a seq task
    val fromArray : 'a array -> 'a seq task

    val toList : 'a seq -> 'a list task
    val toArray : 'a seq -> 'a array task
    val toVector : 'a seq -> 'a vector task
end
