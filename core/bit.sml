structure Bit : ATHENA_WORD =
struct
type word = bool

val wordSize = 1

fun toLarge true = LargeWord.fromInt 1
  | toLarge false = LargeWord.fromInt 0

fun toLargeX true = LargeWord.fromInt (~1)
  | toLargeX false = LargeWord.fromInt 0

fun fromLarge w = LargeWord.andb(w,0w1) <> 0w0

val toLargeWord = toLarge
val toLargeWordX = toLargeX
val fromLargeWord = fromLarge

fun toLargeInt true : IntInf.int = 1
  | toLargeInt false = 0

fun toLargeIntX true : IntInf.int = ~1
  | toLargeIntX false = 0

fun toInt true = 1
  | toInt false = 0

fun fromInt x = (x mod 2) = 1

fun fromLargeInt (x:IntInf.int) = (x mod 2) = 1

fun toIntX true = ~1
  | toIntX false = 0

fun orb (a,b) = a orelse b

fun andb (a,b) = a andalso b

fun notb w = not w

fun xorb (a,b) = (a andalso not b) orelse
		 (b andalso not a)

fun << (v, 0w0) = v
  | << (v, _) = false

val >> = <<

fun ~>> (true, _) = true
  | ~>> (false,_) = false

fun a + b = xorb (a,b)
fun a * b = andb (a,b)
fun a - b = xorb (a,b)
fun _ div false = raise Div
  | a div _ = a

fun _ mod false = raise Div
  | _ mod _ = false

fun compare (true, false) = GREATER
  | compare (false, true) = LESS
  | compare _ = EQUAL

fun a > b = a andalso not b

fun a >= b = a orelse not b

fun a < b = not a andalso b

fun a <= b = not a orelse b

fun min (a,b) = a andalso b

fun max (a,b) = a orelse b

fun scan rad c w = raise Fail "NYI"
fun fromString s = raise Fail "NYI"

fun ~ a = a

fun toString false = "0"
  | toString true = "1"

fun fmt _ = toString

val one = true
val zero = false
val bytes = 1

fun rotl (a,_) = a
fun rotr (a,_) = a
fun bit (a,_) = a
fun byte (true, _) : Word8.word = 0w1
  | byte (false,_) = 0w0

end
