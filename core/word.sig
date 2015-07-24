signature ATHENA_WORD =
sig

include WORD

val one : word
val zero : word
val bytes : int

val rotl : word * Word.word -> word
val rotr : word * Word.word -> word
val bit : word * Word.word -> bool
val byte : word * Word.word -> Word8.word

end
