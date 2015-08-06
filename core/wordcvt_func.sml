structure Endian =
struct
datatype endian = LITTLE | BIG
end

functor WordCvtFn (T : sig
		       structure W1 : ATHENA_WORD
		       structure W2 : ATHENA_WORD
		   end) =
struct

open Endian

structure V = Vector
structure VS = VectorSlice

fun litconv w  = T.W2.fromLargeInt (T.W1.toLargeInt w)

fun wordOf LITTLE (n,w) = 
    litconv (T.W1.>> (w, Word.fromInt (n*T.W2.wordSize)))
  | wordOf BIG (n,w) =
    litconv (T.W1.>> (w, Word.fromInt (T.W1.wordSize-
				       (n+1)*T.W2.wordSize)))
fun tabulate (v,f) = VS.full (V.tabulate (v,f))

local
    open T.W2
    infix << orb
in

fun toWord LITTLE = 
    VS.foldr (fn (w,acc) => (acc << Word.fromInt T.W1.wordSize)
				orb litconv w) zero
  | toWord BIG =
    VS.foldl (fn (w,acc) => (acc << Word.fromInt T.W1.wordSize) 
				orb litconv w) zero 

end


fun conv e =
    case Int.compare (T.W1.wordSize, T.W2.wordSize) of
	EQUAL => (fn x => VS.full (VS.map litconv x))
      | LESS => 
	let val diff = T.W2.wordSize div T.W1.wordSize
	in fn wv =>
	      let val len = (VS.length wv + diff - 1) div diff
	      in tabulate 
		     (len, fn i => 
			      case i = len-1 of
				  true => toWord e (VS.subslice (wv, i*diff, NONE))
				| false =>
				  toWord e (VS.subslice (wv,i*diff, SOME diff)))
	      end
	end
      | GREATER =>
	let val diff = T.W1.wordSize div T.W2.wordSize
	in fn wv =>
	      let val len = VS.length wv * diff
	      in tabulate
		     (len,
		      fn i => wordOf e (i mod diff,
					VS.sub (wv, i div diff)))
	      end
	end
			 

	      
end

