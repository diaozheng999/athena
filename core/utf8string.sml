structure Utf8String =
struct

type char = Word.word
type string = char vector

val maxSize =
    case Int.maxInt of
      SOME i => i
    | NONE => Word.toInt (0wx7FFFFFFF)


val size = Vector.length
val sub = Vector.sub
fun extract s = (VectorSlice.vector o VectorSlice.slice) s
fun substring (s,i,j) = extract(s,i,SOME j)
val concat = Vector.concat
fun i ^ j = concat [i,j]

fun concatWith (s, []) = Vector.fromList []
  | concatWith (s, [x]) = x
  | concatWith (s, x::y::xs) = concatWith (s, concat [x,s,y]::xs)

fun str c = Vector.fromList [c]

val implode = Vector.fromList

fun explode s = Vector.foldr (op::) [] s

val map = Vector.map

fun translate f s = concat (List.map f (explode s))

fun fields f s =



end
