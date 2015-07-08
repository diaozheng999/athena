structure Utf8Char  =
struct

type char = Word8Vector.vector
type string = char vector


type description = Lu | Ll | Lt | Lm | Lo | Mn | Mc | Me | Nd | Nl | No | Pc 
		   | Pd | Ps | Pe | Pi | Pf | Po | Sm | Sc | Sk | So | Zs | Zl 
		   | Zp | Cc | Cf | Cs | Co | Cn

val minChar = Word8Vector.fromList [0w0]

val maxChar = Word8Vector.fromList [0wxF4, 0wx8F, 0wxBF, 0wxBF]

val maxOrd = 0x10FFFF

(* ord : char -> int
   requires : c is a correct UTF-8 representation 
   ensures : ord c evaluates to the utf-8 code point for c *)
fun ord c = 
    let val cmask = Word.fromInt o Word8.toInt o Word8.andb
    in
	case Word8Vector.length c of
	    1 => Word8.toInt (Word8Vector.sub (c,0))
	  | size =>
	    Word.toInt
		(Word8Vector.foldli
		     (fn (0, byte, acc) =>
			 (case size of
			      2 => cmask (byte, 0wx1F)
			    | 3 => cmask (byte, 0wx0F)
			    | 4 => cmask (byte, 0wx07)
			    | 5 => cmask (byte, 0wx03)
			    | _ => cmask (byte, 0wx01))
		     | (_, byte, acc) => Word.orb (Word.<< (acc, 0w6),
						   cmask (byte, 0wx3F)))
		     0w0 c)
    end


(* chr : int -> char
   requires : i >= 0
   ensures : chr i evaluates to the utf-encoded character given code point i *)
fun chr i =
    if i <= 127 then Word8Vector.tabulate (1, fn _ => Word8.fromInt i)
    else
	let val wi = Word.fromInt i
	    val cmask = Word8.fromInt o Word.toInt o Word.orb
	    fun build word 0 acc = (word, acc)
	      | build word n acc = build 
				       (Word.>> (word, 0w6))
				       (n-1)
				       (cmask (Word.andb (0wx3F, word), 0wx80)::acc)
	in if i<=2047 then
	       let val (word, acc) = build wi 1 []
	       in Word8Vector.fromList (cmask (word, 0wxC0)::acc) end
	   else if i<=65535 then
	       let val (word, acc) = build wi 2 []
	       in Word8Vector.fromList (cmask (word, 0wxE0)::acc) end
	   else if i<=2097151 then
	       let val (word, acc) = build wi 3 []
	       in Word8Vector.fromList (cmask (word, 0wxF0)::acc) end
	   else if i<=67108863 then
	       let val (word, acc) = build wi 4 []
	       in Word8Vector.fromList (cmask (word, 0wxF8)::acc) end
	   else 
	       let val (word, acc) = build wi 5 []
	       in Word8Vector.fromList (cmask (word, 0wxFC)::acc) end
	end

(* succ : char -> char 
   requires : true
   ensures : succ c returns the next character. raises Chr if chr = maxChar *)
fun succ c = 
    let val i = ord c
    in case Int.compare (i, maxOrd) of
	   LESS => chr (i+1)
	 | _ => raise Chr
    end


(* pred : char -> char
   requires : true
   ensures : pred c returns the previous character. raises Chr if chr = minChar *)
fun pred c =
    let val i = ord c
    in case Int.compare (i, 0) of
	   GREATER => chr (i-1)
	 | _ => raise Chr
    end

(* compareSlice : Word8VectorSlice * Word8VectorSlice -> order 
   requires : a and b are the same length
   ensures : compareSlice(a,b) compares a and b in byte ordering*)
fun compareSlice (a,b) = 
    case (Word8.compare (Word8VectorSlice.sub(a, 0),
			 Word8VectorSlice.sub(b, 0)),
	  Word8VectorSlice.length a) of
	(EQUAL, 1) => EQUAL
      | (EQUAL, _) => compareSlice (Word8VectorSlice.subslice(a,1,NONE),
				    Word8VectorSlice.subslice(b,1,NONE))
      | (cmp, _) => cmp


fun compare (a,b) =
    case Int.compare (Word8Vector.length a, Word8Vector.length b) of
	EQUAL => compareSlice (Word8VectorSlice.full a,
			       Word8VectorSlice.full b)
      | cmp => cmp


fun a < b = 
    case compare(a,b) of
	LESS => true
      | _ => false

fun a <= b =
    case compare(a,b) of
	GREATER => false
      | _ => true

fun a > b =
    case compare(a,b) of
	GREATER => true
      | _ => false

fun a >= b =
    case compare(a,b) of
	LESS => false
      | _ => true

fun contains s =
    let val p = Vector.foldl (IntRedBlackSet.add') IntRedBlackSet.empty
			     (Vector.map ord s)
    in fn c =>IntRedBlackSet.member (p, ord c) end

fun notContains s =
    let val p = contains s
    in fn c => not (p c) end

fun isAscii c = Word8Vector.length c = 1



end
