structure Stack : STACK =
struct

type 'a stack = 'a list

fun empty () = []

val isEmpty = List.null

fun push s e = e::s

fun pop [] = (NONE, [])
  | pop (x::xs) = (SOME x, xs)

fun rev' [] os = os
  | rev' (x::xs) os = rev' xs (x::os)

fun rev s = rev' s []

fun clone s = s

fun toList s = s

fun toArray s = Array.fromList s

fun toVector s = Vector.fromList s

end

structure FunStack : STACK =
struct 
datatype 'a stack = EMPTY | STACK of unit -> 'a option * 'a stack

fun empty () = EMPTY

fun isEmpty EMPTY = true
  | isEmpty _ = false

fun push s e = STACK (fn () => (SOME e, s))
		     
fun pop EMPTY = (NONE, EMPTY)
  | pop (STACK s) = s ()


fun rev' EMPTY os = os
  | rev' is os = case pop is of
		     (SOME e, is') => rev' is' (push os e)
		   | _ => raise Fail ""


fun rev s = rev' s (empty ())

fun clone s = s

fun toList EMPTY = []
  | toList s = case pop s of
		   (SOME e, t) => e::toList t
		 | _ => raise Fail ""

fun toArray s = (Array.fromList o toList) s

fun toVector s = (Vector.fromList o toList) s

end
