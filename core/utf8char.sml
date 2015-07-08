structure Utf8Char :> CHAR =
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


fun readn n getc strm =
    let fun readn' 0w0 strm acc = SOME (acc, strm)
          | readn' n strm acc =
            case getc strm of
              SOME (c, strm') => readn' (n-0w1) strm' (acc ^ (String.str c))
            | NONE => NONE
    in readn' n strm "" end



fun stringReader "" = NONE
  | stringReader s =
    SOME (String.sub (s,0), String.extract (s,1,NONE))


fun scan (getc:(Char.char, 'a) StringCvt.reader) (strm:'a) =
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

    (* invalid 2-byte characters *)
    | SOME(#"\192", rest) => NONE
    | SOME(#"\193", rest) => NONE

    (* check for already used characters *)
    | SOME(c, rest) =>

    (* check for byte-order-mark *)
      (case readn 0w3 getc strm of
         SOME (s, rest') =>
         if s="\239\187\191" then scan getc rest'
         else

    (* check for unescaped UTF-8 sequences *)
           let val w = Byte.charToByte c
               val cmask = fromInt o Word8.toInt o Word8.andb
               exception p
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
                      in case c>=maxChar of
                           true => NONE
                         | false => SOME (c, rest)
                      end handle p => NONE)
                   | NONE => NONE

           in case (Word8.andb (w,0wx80),
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
           end
       | NONE => NONE)
    | NONE => NONE


fun fromString s =
    case scan stringReader s of
      SOME (c, _) => SOME c
    | NONE => NONE

val fromCString = fromString


fun toCString c =
    let val s = Word.toString c in
      case String.size s of
        1 => "\\u000"^s
      | 2 => "\\u00"^s
      | 3 => "\\u0"^s
      | _ => "\\u"^s
    end


val scan : (char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader =
 fn getc =>
    let fun getc' strm =
            case getc strm of
              SOME (c,r) => SOME (Char.chr (ord c), r)
            | NONE => NONE
    in fn strm => scan getc' strm end

end
