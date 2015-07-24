functor ApplyRotFn (W:WORD) : ATHENA_WORD =
struct

val size = Word.fromInt W.wordSize
val one = W.fromInt 1
val zero = W.fromInt 0
val bytes = (W.wordSize + 7) div 8
val bmask = W.fromInt 256

open W
infix << >> orb andb

fun rotl (w,n) = (w << n) orb (w >> Word.-(size,n))
fun rotr (w,n) = (w >> n) orb (w << Word.-(size,n))
fun bit (w,n) = 
    case compare ((w andb (one << n)),zero) of
	EQUAL => false
      | _ => true
fun byte (w,n) = 
    (Word8.fromLargeWord o toLargeWord) 
	((w >> (Word.*(n, 0w8))) andb bmask)
end

structure AthenaWord :> ATHENA_WORD
		      where type word = Word.word
= ApplyRotFn (Word)

structure AthenaWord8 :> ATHENA_WORD
		       where type word = Word8.word
= ApplyRotFn (Word8)

structure AthenaWord32 :> ATHENA_WORD
			where type word = Word32.word
= ApplyRotFn (Word32)

structure AthenaWord64 :> ATHENA_WORD
			where type word = Word64.word
= ApplyRotFn (Word64)
		       
