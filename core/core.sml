structure Core : CORE =
struct

type 'a cont = unit -> 'a
datatype 'a loopguard = LOOPGUARD of 'a vector


fun delay obj () = obj

fun expose delayed = delayed ()

fun curry f a b = f (a, b)

fun uncurry f (a,b) = f a b

fun min (a,b) = if a<b then a else b

fun max (a,b) = if a<b then b else a

fun for v = LOOPGUARD v

fun loop (LOOPGUARD v, f) = Vector.app f v

fun to (init, fin) = Vector.tabulate (fin-init+1, fn i=>init+i)

fun car (a, _) = a

fun cdr (_, a) = a

fun fst (f, b) a = f (a, b)

fun snd (f, a) b = f (a, b)

val u = Option.valOf o Utf8String.fromString
val % = Option.valOf o Utf8Char.fromString
val ^^ = Utf8String.^
val u_chr = Utf8Char.chr
val u_concat = Utf8String.concat
val u_explode = Utf8String.explode
val u_implode = Utf8String.implode
val u_ord = Utf8Char.ord
val u_size = Utf8String.size
val u_str = Utf8String.str
val u_substring = Utf8String.substring

end
