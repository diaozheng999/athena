functor ApplyRotFn (T : sig
			structure W : WORD
			structure WW : MLTON_WORD
			where type t = W.word
		    end) : ATHENA_WORD =
struct

open T

val size = Word.fromInt W.wordSize
val one = W.fromInt 1
val zero = W.fromInt 0
val bytes = (W.wordSize + 7) div 8
val bmask = W.fromInt 256

open W
infix << >> orb andb

fun rotl f = WW.rol f
fun rotr f = WW.ror f

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
= ApplyRotFn (struct
	       structure W = Word
	       structure WW = MLton.Word
	       end)

structure AthenaWord8 :> ATHENA_WORD
			     where type word = Word8.word
= ApplyRotFn (struct
	       structure W = Word8
	       structure WW = MLton.Word8
	       end)

structure AthenaWord32 :> ATHENA_WORD
			      where type word = Word32.word
= ApplyRotFn (struct
	       structure W = Word32
	       structure WW = MLton.Word32
	       end)

structure AthenaWord64 :> ATHENA_WORD
			      where type word = Word64.word
= ApplyRotFn (struct
	       structure W = Word64
	       structure WW = MLton.Word64
	       end)
