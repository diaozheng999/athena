signature CORE = sig
			       
    type 'a loopguard

    (* as the name suggests, delay f delays f *)
    val delay : 'a -> unit -> 'a

    (* expose exposes a delayed component *)
    val expose : (unit -> 'a) -> 'a

    (* curry takes a function with tupled argument and turns it
       into a curried function *)
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

    (* uncurry takes a function with curreied argument and turns
       it into a tupled function *)
    val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c

    (* returns the min of two ints *)
    val min : int * int -> int

    (* returns the max of two ints *)
    val max : int * int -> int


    (* loop, for, to creates a shorthand to write for loops
       given the proper infix notations, we can write:

       for (0 to 10) loop (fn x => ... )

    *)
    val loop : 'a loopguard * ('a -> unit) -> unit
    val for : 'a vector -> 'a loopguard
    val to : (int * int) -> int vector

    (* 'nuff said. *)
    val car : 'a * 'b -> 'a
    val cdr : 'a * 'b -> 'b

    (* short hands for functions with one element already applied *)
    val fst : ('a * 'b -> 'c) * 'b -> 'a -> 'c
    val snd : ('a * 'b -> 'c) * 'a -> 'b -> 'c
end
