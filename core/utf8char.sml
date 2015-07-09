structure Utf8Char :> ATHENA_CHAR
			  where type char = Word.word
			  where type string = Word.word vector
=
struct

open Utf8Helper
open Word

type char = word
type string = char vector

val minChar = 0w0
val maxChar = 0wx10ffff
val maxOrd = 0x10ffff

val ord = toInt
val chr = fromInt



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


val toChar = Char.chr o ord
val fromChar = chr o Char.ord

fun char f = fromChar o f o toChar

(* the following functions depends on libicu *)
(* TODO: implement the following functions *)
val toLower = char Char.toLower
val toUpper = char Char.toUpper
fun isAlpha c = isAscii c andalso Char.isAlpha (toChar c)
fun isAlphaNum c = isAscii c andalso Char.isAlphaNum (toChar c)
fun isCntrl c = isAscii c andalso Char.isCntrl (toChar c)
fun isDigit c = isAscii c andalso Char.isDigit (toChar c)
fun isGraph c = isAscii c andalso Char.isGraph (toChar c)
fun isHexDigit c = isAscii c andalso Char.isHexDigit (toChar c)
fun isLower c = isAscii c andalso Char.isLower (toChar c)
fun isPrint c = isAscii c andalso Char.isPrint (toChar c)
fun isSpace c = isAscii c andalso Char.isSpace (toChar c)
fun isPunct c = isPunct c andalso Char.isPunct (toChar c)
fun isUpper c = isUpper c andalso Char.isUpper (toChar c)



fun toString i =
    if i <= 0w127 then Char.toString (Char.chr (ord i))
    else
      let
        val cmask = Byte.byteToChar o Word8.fromInt o Word.toInt o Word.orb
        fun build word 0w0 acc = (word, acc)
          | build word n acc =
            build
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




fun scan getc strm  =
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
                 (GREATER, LESS) => SOME (chr (Int.- (i,64)), rest')
               | _ => NONE end
          | NONE => NONE)
       | SOME (#"u", rest') =>
         (case readn 0w4 getc rest' of
            SOME (p, r) =>
            (case Word.scan StringCvt.HEX stringReader p of
               SOME (w, _) => (case (compare (w, minChar),
                                     compare (w, maxChar)) of
                                 (LESS, _) => NONE
                               | (_, GREATER) => NONE
                               | _ => SOME (w, r))
             | NONE => NONE)
          | NONE => NONE)
       | SOME (#"x", rest') =>
         (case readn 0w2 getc rest' of
            SOME (p, r) =>
            (case Word.scan StringCvt.HEX stringReader p of
               SOME (w, _) => SOME (w, r)
             | NONE => NONE)
          | NONE => NONE)
       | SOME (_, _) =>
         (case readn 0w3 getc rest of
            SOME (p, rest') =>
            if CharVector.all Char.isDigit p then
              (case Int.scan StringCvt.DEC stringReader p of
                 SOME (v, _) => SOME (chr v, rest')
               | NONE => NONE)
            else NONE
          | NONE => NONE)
       | NONE => NONE)

    (* check for already used characters *)
    | SOME(c, rest) =>

    (* check for unescaped UTF-8 sequences *)
      let val w = Byte.charToByte c
          val cmask = fromInt o Word8.toInt o Word8.andb
          exception p

	  fun overlongGuard word 1 = word >= 0w0 andalso word <= 0w127
	    | overlongGuard word 2 = word >= 0w128 andalso word <= 0w2047
	    | overlongGuard word 3 = word >= 0w2048 andalso word <= 0w65535
	    | overlongGuard word 4 = word >= 0w65536 andalso word <= 0w2097151
	    | overlongGuard word 5 = word >= 0w2097152 andalso word <= 0w67108863
	    | overlongGuard word 6 = word >= 0w67108864
	    | overlongGuard word _ = false

          fun makeWord size strm =
              case readn (fromInt size) getc strm of
                  SOME (s, rest) =>
                  (let
                      val c =
                          (CharVector.foldli
                               (fn (0, byte, acc) =>
                                   (case size of
                                        2 => cmask (Byte.charToByte byte, 0wx1F)
                                      | 3 => cmask (Byte.charToByte byte, 0wx0F)
                                      | 4 => cmask (Byte.charToByte byte, 0wx07)
                                      | 5 => cmask (Byte.charToByte byte, 0wx03)
                                      | _ => cmask (Byte.charToByte byte, 0wx01)
                                   )
                               | (_, byte, acc) =>
				 
				 (* each subsequent byte has header 0b10xxxxxx *)
                                 if Word8.andb(Byte.charToByte byte,
                                               0wxC0)<>0wx80
                                 then raise p
                                 else
                                     Word.orb (Word.<< (acc, 0w6),
                                               cmask (Byte.charToByte byte,
                                                      0wx3F)))
                               0w0 s)
                  in case overlongGuard c size andalso c<=maxChar of
                         true => SOME (c, rest)
                       | false => NONE
                  end handle p => NONE)
                | NONE => NONE
			     
	  fun exec () = case (Word8.andb (w,0wx80),
			      Word8.andb (w,0wxE0),
			      Word8.andb (w,0wxF0),
			      Word8.andb (w,0wxF8),
			      Word8.andb (w,0wxFC),
			      Word8.andb (w,0wxFE)) of
			    (0wx00,_,_,_,_,_) => SOME (chr (Char.ord c), rest)
			  | (_,0wxC0,_,_,_,_) => makeWord 2 strm
			  | (_,_,0wxE0,_,_,_) => makeWord 3 strm
			  | (_,_,_,0wxF0,_,_) => makeWord 4 strm
			  | (_,_,_,_,0wxF8,_) => makeWord 5 strm
			  | (_,_,_,_,_,0wxFC) => makeWord 6 strm
			  | _ => NONE
				      
				      
      in 

    (* check for byte-order-mark *)
      (case readn 0w3 getc strm of
         SOME (s, rest') =>
         if s="\239\187\191" then scan getc rest'
         else exec ()
	| NONE => exec ())
      end
  | NONE => NONE

 

fun fromString s =
    case scan stringReader s of
      SOME (c, _) => SOME c
    | NONE => NONE

val fromCString = fromString

val toCString = String.toCString o toString

(*
val scan : (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader =
 fn getc =>
    let fun getc' strm =
            case getc strm of
              SOME (c,r) => SOME (Char.chr (ord c), r)
            | NONE => NONE
    in fn strm => scan getc' strm end
*)


fun d__testCases param =
    case param of
	Env.DEBUG =>
	let
	    (* basis test cases *)
	    val NONE = fromString "\\q"
	    val SOME 0w97 = fromString "a\^D"
	    val SOME 0w97 = fromString "a\\ \\q"
	    val NONE = fromString "\\ \\"
	    val NONE = fromString ""
	    val NONE = fromString "\\ \\\^D"
	    val NONE = fromString "\\ a"

	    (* ascii escape sequences tests *)
	    val SOME 0wx07 = fromString "\\a"
	    val SOME 0wx08 = fromString "\\b"
	    val SOME 0wx07 = fromString "\a"
	    val SOME 0wx08 = fromString "\\b"

	    (* ascii control sequence tests *)
	    val SOME 0wx08 = fromString "\^H"
	    val SOME 0wx08 = fromString "\\^H"
	    val SOME 0w0 = fromString "\^@"
	    val SOME 0w0 = fromString "\\^@"
	    val NONE = fromString "\\^?"
	    val NONE = fromString "\\^`"

	    (* unicode BOM tests *)
	    val SOME 0w97 = fromString "\239\187\191a"
	    val SOME 0wx07 = fromString "\239\187\191\\a"

	    (* ascii positive tests *)
	    val SOME 0w65 = fromString "A"
	    val SOME 0w63 = fromString "?"

	    (* invalid unicode *)
	    val NONE = fromString "\192\191"
	    val NONE = fromString "\193\191"
	    val NONE = fromString "\244\144\128\128"
	    val NONE = fromString "\144"
	    val NONE = fromString "\128"
	    val NONE = fromString "\240\130\130\172"
	    val NONE = fromString "\230\177"
	    val NONE = fromString "\230\127\137"
	    
	    (* valid unicode *)
	    val SOME 0wx20AC = fromString "\226\130\172"
	    val SOME 0w27721 = fromString "\230\177\137"
	    

	    (* invalid unicode escape sequence *)
	    val NONE = fromString "\\u0"
	    val NONE = fromString "\\u000"
	    val SOME 0wx1100 = fromString "\\u110000" (*note only first 4 bytes are read*)
 	in () end
      | _ => ()

val () = d__testCases Env.debug
end
