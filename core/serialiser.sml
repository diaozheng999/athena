structure Serialiser : SERIALISER =
struct

structure arr = Word8Array
structure arrs = Word8ArraySlice

type serialised = Word8ArraySlice.slice

exception Type

(* Header definitions
  00 - Unit
  01 - Bool
  02 - Char
  03 - Int
  04 - Real
  05 - String
  06 - LargeInt
  07 - List
  08 - Vector
  09 - Array
  0A - Order
  0B - Option
  FA - LargeInt (negative)
  FD - Int (negative)
  FF - Serialised
*)

val null : Word8.word = 0wx0

val tabulate = arrs.full o arr.tabulate

val bits : Word8Vector.vector = Word8Vector.fromList [0wx80,0wx40,0wx20,0wx10,0wx08,0wx04,0wx02,0wx01]

fun packInt i =
    case Int.compare(i,0) of
	LESS => let val arr = packInt (~i)
		    val () = arrs.update (arr, 0, 0wxfd)
		in arr end
      | _ => tabulate (5,
		       fn 0 => 0wx03
		       | 1 => Word8.fromInt i
		       | 2 => Word8.fromInt (i div 256)
		       | 3 => Word8.fromInt (i div 65536)
		       | 4 => Word8.fromInt (i div 16777216)
		       | _ => raise Type)


fun packLargeInt i =
    case IntInf.compare(i,0) of
	LESS => let val arr = packLargeInt (~i)
		    val () = arrs.update (arr, 0, 0wxfa)
		in arr end
      | _ => let val wi = LargeWord.fromLargeInt i
		 fun size (0,n) = n
		   | size (x,n) = size (x div 256, n+1)
		 val int_size : int = size (i,0)
		 val arr = arr.array (int_size+6, 0wx06)
		 fun push (x,n) =
		     (arr.update (arr,n,(Word8.fromLargeInt x));
		      case IntInf.compare (x,256) of
			  LESS => ()
			| _ => push (x div 256, n+1))

		 val () = push (i, 6)
		 val () = arrs.copy {src=packInt int_size,dst=arr,di=1}
	     in arrs.full arr end

fun packBool true = tabulate (2, fn 0 => 0wx01 | 1 => 0wx01 | _ => raise Type)
  | packBool false =tabulate (2, fn 0 => 0wx01 | 1 => 0wx00 | _ => raise Type)

fun packUnit () =  tabulate (1, fn _ => 0wx00)

fun packChar c = tabulate (2, fn 0 => 0wx02 | 1 => (Word8.fromInt (ord c)) | _ => raise Type)

fun packString s =
    let val len = String.size s
	val arr = arr.tabulate (len+6,
				fn i => case Int.compare (i, 6) of
					    LESS => 0wx05
					  | _ => (Word8.fromInt (ord (String.sub (s,i-6)))))
	val () = arrs.copy {src=packInt len, dst=arr, di=1}
    in arrs.full arr end

fun packReal r =
    let val arr = packString (Real.toString r)
	val arr2 = arr.tabulate (arrs.length arr+1, fn 0 => 0wx04
						  | n => arrs.sub(arr,n-1))
    in arrs.full arr2 end


fun packList serialiser [] = tabulate (2, fn 0 => 0wx07 |  _ => 0wx00)
  | packList serialiser (x::xs) = 
    let val rest = packList serialiser xs
	val elem = serialiser x
	val size = arrs.length elem
	val arr = arr.tabulate (arrs.length rest+2+size,
				fn 0 => 0wx07 | 1 => 0wx01
				| n => case Int.compare (n, size+2) of
					   LESS => arrs.sub(elem,n-2)
					 | _ => arrs.sub(rest,n-size-2))
    in arrs.full arr end

fun packVector serialiser v =
    let val sl = Vector.map serialiser v
	val size = Vector.foldl (fn (s,n) => n+arrs.length s) (6+5*Vector.length v) sl
	val arr = arr.array (size, 0wx08)
	val _ = Vector.foldli (fn (i,s,n) =>(arrs.copy {src=s, dst=arr,di=n};
					     arrs.copy {src=packInt n,dst=arr,di=6+i*5};
					     n+arrs.length s)) (6+5*Vector.length v) sl
	val () = arrs.copy {src=packInt (Vector.length v),dst=arr,di=1}
    in arrs.full arr end
			       
fun packArray serialiser v =
    let val arr = packVector serialiser (Array.vector v)
	val arr2 = arr.array (arrs.length arr+1, 0wx09)
	val () = arrs.copy{src=arr, dst=arr2, di=1}
    in arrs.full arr2 end

fun packOrder LESS = tabulate(2, fn 0 => 0wx0a | _ => 0wxff)
  | packOrder EQUAL = tabulate (2, fn 0 => 0wx0a | _ => 0wx00)
  | packOrder GREATER = tabulate (2, fn 0 => 0wx0a | _ => 0wx01)

fun packOption serialiser NONE = tabulate (2, fn 0 => 0wx0b | _ => 0wx00)
  | packOption serialiser (SOME a) =
    let val arr = serialiser a
	val arr2 = arr.tabulate (arrs.length arr+2, fn 0 => 0wx0b | _ => 0wx01)
	val () = arrs.copy {src=arr,dst=arr2,di=2}
    in arrs.full arr2 end

fun unpackOrder s = 
    case (arrs.sub(s,0),arrs.sub(s,1)) of
	(0wx0a,0wxff) => LESS
      | (0wx0a,0wx00) => EQUAL
      | (0wx0a,0wx01) => GREATER
      | _ => raise Type

fun unpackOption unpack s =
    case (arrs.sub(s,0),arrs.sub(s,1)) of
	(0wx0b,0wx00) => NONE
      | (0wx0b,0wx01) => SOME (unpack (arrs.subslice(s,2,NONE)))
      | _ => raise Type

fun unpackInt s =
    case arrs.sub(s,0) of 
	0wx03 => arrs.foldr (fn (e,v) => v*256+(Word8.toInt e)) 0 (arrs.subslice (s,1,SOME 4))
      | 0wxfd => arrs.foldr (fn (e,v) => v*256-(Word8.toInt e)) 0 (arrs.subslice (s,1,SOME 4))
      | _ => raise Type

fun unpackBool s =
    case (arrs.sub(s,0),arrs.sub(s,1)) of
	(0wx01,0wx01) => true
      | (0wx01,0wx00) => false
      | _ => raise Type

fun unpackLargeInt s = 
    let val j = unpackInt (arrs.subslice(s,1,NONE))
    in case arrs.sub(s,0) of
	   0wx06 => arrs.foldr (fn (e,v) => v*256+(Word8.toLargeInt e)) 0 (arrs.subslice (s,6,SOME j))
	 | 0wxfa => arrs.foldr (fn (e,v) => v*256-(Word8.toLargeInt e)) 0 (arrs.subslice (s,6,SOME j))
	 | _ => raise Type
    end

fun unpackUnit s =
    case arrs.sub(s,0) of
	0wx00 => ()
      | _ => raise Type

fun unpackChar s =
    case arrs.sub(s,0) of
	0wx02 => chr (Word8.toInt (arrs.sub (s, 1)))
      | _ => raise Type

fun unpackString s =
    case arrs.sub(s,0) of
	0wx05 => CharVector.tabulate (unpackInt (arrs.subslice(s,1,NONE)),
				      fn i => chr(Word8.toInt (arrs.sub(s,i+6))))
      | _ => raise Type

fun unpackReal s =
    case arrs.sub(s,0) of
	0wx04 => (case Real.fromString (unpackString (arrs.subslice(s,1,NONE))) of
		      SOME v => v
		    | _ => raise Type)
      | _ => raise Type


fun unpackList (unpack, size) s =
    case (arrs.sub(s,0), arrs.sub(s,1)) of
	(0wx07,0wx00) => []
      | (0wx07,0wx01) => let
	  val len = size (arrs.subslice(s,2,NONE))
	  val elem = unpack (arrs.subslice(s,2,NONE))
      in elem :: (unpackList (unpack, size) (arrs.subslice(s,len+2,NONE))) end
      | _ => raise Type


fun unpackVectorLength s =
    case arrs.sub(s,0) of
	0wx08 => unpackInt (arrs.subslice(s,1,NONE))
      | _ => raise Type

fun unpackVectorElement unpack s i =
    case (Int.compare(0,i),Int.compare(0,unpackVectorLength s)) of
	(GREATER, _) => raise Subscript
      | (_, LESS) => unpack(arrs.subslice(s, unpackInt(arrs.subslice(s,i*5+6,NONE)),NONE))
      | _ => raise Subscript


fun unpackVector unpack s =
    let val len = unpackVectorLength s
    in Vector.tabulate (len, fn i => unpackVectorElement unpack s i) end


fun unpackArrayLength s =
    case arrs.sub(s,0) of
	0wx09 => unpackVectorLength (arrs.subslice(s,1,NONE))
      | _ => raise Type

fun unpackArrayElement unpack s = unpackVectorElement unpack (arrs.subslice(s,1,NONE))

					
fun unpackArray unpack s =
    let val len = unpackArrayLength s
    in Array.tabulate (len, fn i=> unpackArrayElement unpack s i) end
    

fun toString s = arrs.foldl (fn (b,s) => case Word8.compare (b, 0wx10) of
					     LESS => s^"0"^(Word8.toString b)^" "
					   | _ => s^(Word8.toString b)^" ") "" s


fun sizeInt s = case arrs.sub(s,0) of 0wx03 => 5 | _ => raise Type
fun sizeLargeInt s = case arrs.sub(s,0) of 0wx06 => (sizeInt (arrs.subslice(s,1,NONE)))+6 
					 | _ => raise Type
fun sizeBool s = case arrs.sub(s,0) of 0wx01 => 2 | _ => raise Type
fun sizeUnit s = case arrs.sub(s,0) of 0wx00 => 1 | _ => raise Type
fun sizeChar s = case arrs.sub(s,0) of 0wx02 => 2 | _ => raise Type
fun sizeString s = case arrs.sub(s,0) of 0wx05 => (sizeInt (arrs.subslice(s,1,NONE)))+6
					| _ => raise Type
fun sizeReal s = case arrs.sub(s,0) of 0wx04 => (sizeString (arrs.subslice(s,1,NONE)))+1
				     | _ => raise Type


fun sizeList size s = 
    case (arrs.sub(s,0),arrs.sub(s,1)) of
	(0wx07,0wx00) => 2
      | (0wx07,0wx01) => let val sz = size (arrs.subslice(s,2,NONE))
			 in sz+2+sizeList size (arrs.subslice(s,2,NONE)) end
      | _ => raise Type


fun sizeVector size s =
    case arrs.sub(s,0) of
	0wx08 => let val len = unpackInt (arrs.subslice(s,1,NONE))
		     val li = unpackInt (arrs.subslice(s,1+len*5,NONE))
		     val () = print ("len "^Int.toString len^" li "^Int.toString li^"\n")
		 in li+size (arrs.subslice(s,li,NONE)) end
      | _ => raise Type

fun sizeVectorElement size s i =
    case (Int.compare (~1,i), Int.compare(i,unpackVectorLength s)) of
	(LESS, LESS) => let val li = unpackInt (arrs.subslice(s,6+i*5,NONE))
			in size(arrs.subslice(s,li,NONE)) end
      | _ => raise Subscript

fun sizeArray size s =
    case arrs.sub(s,0) of
	0wx09 => sizeVector size (arrs.subslice(s,1,NONE)) +1 
      | _ => raise Type

fun sizeArrayElement size s i =
    case (Int.compare (~1,i), Int.compare(i,unpackArrayLength s)) of
	(LESS,LESS) => sizeVectorElement size (arrs.subslice(s,1,NONE)) i
      | _ => raise Subscript


fun sizeOrder s = case arrs.sub(s,0) of 0wx0a => 2 | _ => raise Type
fun sizeOption size s = 
    case (arrs.sub(s,0),arrs.sub(s,1)) of
	(0wx0b,0wx00) => 2
      | (0wx0b,0wx01) => 2+size (arrs.subslice(s,2,NONE))
      | _ => raise Type


fun toBitVector s =
    Vector.tabulate (arrs.length s*8,
		     (fn i =>
			 let val (n,b) = (i div 8, i mod 8)
			 in case Word8.andb(arrs.sub(s,n), Word8Vector.sub(bits,b)) of
				0wx0 => false
			      | _ => true end))


fun bitVectorToString b = Vector.foldr (op ^) "" (Vector.map (fn true => "1" | false => "0") b)
end




signature SERIALISABLE =
sig
    type t
     
    val serialise : t -> Serialiser.serialised
    val unserialise : Serialiser.serialised -> t
    val size : Serialiser.serialised -> int

end
