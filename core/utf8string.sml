structure Utf8String :> STRING =
struct

open Core


eqtype string = int * Word8Vector.vector
eqtype char = int * Word8Vector.vector

val maxSize = min(Int.maxInt, CharVector.maxLen)

fun size (len, _) = len

fun sub


end
