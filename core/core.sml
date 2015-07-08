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


end
