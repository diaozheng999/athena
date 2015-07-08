structure Utf8Char =
struct

open Word

type char = word
type string = char vector

val minChar = 0w0

val maxChar = 0wx10ffff

val maxOrd = 0x10ffff

fun ord c = toInt c

fun chr i = fromInt i

fun succ c = case compare (c, maxChar) of
		 LESS => c+0w1
	       | _ => raise Chr

fun pred c = case compare (c, minChar) of
		 GREATER => c-0w1
	       | _ => raise Chr


fun contains s = 
    let val p = Vector.foldl (IntRedBlackSet.add') IntRedBlackSet.empty
			     (Vector.map ord s)
    in fn c => IntRedBlackSet.member (p, ord c) end

fun notContains s =
    let val p = contains s
    in fn c => not (p c) end


fun isAscii c = c <= 0w127

(* the following functions depends on libicu *)
(* TODO: implement the following functions *)
fun toLower c = c
fun toUpper c = c
fun isAlpha c = false
fun isAlphaNum c = false
fun isCntrl c = false
fun isDigit c = false
fun isGraph c = false
fun isHexDigit c = false
fun isLower c = false
fun isPrint c = false
fun isSpace c = false
fun isPunct c = false
fun isUpper c = false



fun toString i =
    if i <= 0w127 then Char.toString (Char.chr (ord i))
    else
	let 
	    val cmask = Byte.byteToChar o Word8.fromInt o Word.toInt o Word.orb
	    fun build word 0w0 acc = (word, acc)
	      | build word n acc = build 
				       (Word.>> (word, 0w6))
				       (n-0w1)
				       (cmask (Word.andb (0wx3F, word), 0wx80)::acc)
	in if i<=0w2047 then
	       let val (word, acc) = build i 0w1 []
	       in CharVector.fromList (cmask (word, 0wxC0)::acc) end
	   else if i<=0w65535 then
	       let val (word, acc) = build i 0w2 []
	       in CharVector.fromList (cmask (word, 0wxE0)::acc) end
	   else if i<=0w2097151 then
	       let val (word, acc) = build i 0w3 []
	       in CharVector.fromList (cmask (word, 0wxF0)::acc) end
	   else if i<=0w67108863 then
	       let val (word, acc) = build i 0w4 []
	       in CharVector.fromList (cmask (word, 0wxF8)::acc) end
	   else 
	       let val (word, acc) = build i 0w5 []
	       in CharVector.fromList (cmask (word, 0wxFC)::acc) end
	end


fun readn n getc strm =
    let fun readn' 0 strm acc = SOME (acc, strm)
	  | readn' n strm acc = 
	    case getc strm of
		SOME (c, strm') => readn' (n-1) strm (acc ^ (Char.toString c))
	      | NONE => NONE
    


fun scan getc strm =
    case getc strm of
	
	(* escape sequences *)
	SOME (#"\\", rest) => 
	(case getc rest of
	     SOME (#"a", rest') => SOME (0wx07, rest')
	   | SOME (#"b", rest') => SOME (0wx08, rest')
	   | SOME (#"t", rest') => SOME (0wx09, rest')
	   | SOME (#"n", rest') => SOME (0wx0A, rest')
	   | SOME (#"v", rest') => SOME (0wx0B, rest')
	   | SOME (#"f", rest') => SOME (0wx0C, rest')
	   | SOME (#"r", rest') => SOME (0wx0D, rest')
	   | SOME (#"\\", rest') => SOME (0w92, rest')
	   | SOME (#"\"", rest') => SOME (0w34, rest')
	   | SOME (#"'", rest') => SOME (0w39, rest')
	   | SOME (#"^", rest') => 
	     (case getc rest' of
		  SOME (c, rest') =>
		  let val i = Char.ord c
		  in case (Int.compare (i,63), Int.compare (i,96)) of
			 (GREATER, LESS) => SOME (chr (i-64), rest')
		       | _ => NONE end
		| NONE => NONE)
	   | SOME (#"u", rest') => 
	     (case readn 4 rest' of
		  SOME (p, rest') => 
		  (case Word.fromString p of
		       SOME w => (case (compare (w, minWord), compare (w, maxWord)) of
				      (LESS, _) => NONE
				    | (_, GREATER) => NONE
				    | _ => SOME (w, rest'))
		     | NONE => NONE)
		  | NONE => NONE)
	   | SOME (_, _) => 
	     (case readn 3 rest of
		  SOME (p, rest') => 
		  if CharVector.all Char.isDigit p then
		      (case Int.fromString p of
			   SOME v => SOME (chr v, rest')
			 | NONE => NONE)
		  else NONE
		| NONE => NONE)
	   | NONE => NONE)

      (* others *) 
      | SOME(c, rest) => SOME(chr (Char.ord c), rest)
      | NONE => NONE




end
