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


    (* array and vector shorthands *)
    val V: 'a list -> 'a vector
    val A: 'a list -> 'a array

    (* utf-8 string functions *)
    val u : string -> Utf8String.string
    val % : string -> Utf8Char.char
    val ^^ : Utf8String.string * Utf8String.string -> Utf8String.string     val u_chr : int -> Utf8Char.char
    val u_concat : Utf8String.string list -> Utf8String.string
    val u_explode : Utf8String.string -> Utf8Char.char list
    val u_implode : Utf8Char.char list -> Utf8String.string
    val u_ord : Utf8Char.char -> int
    val u_size : Utf8String.string -> int
    val u_str : Utf8Char.char -> Utf8String.string
    val u_substring : Utf8String.string * int * int -> Utf8String.string

    val debug : string * string -> unit

end
