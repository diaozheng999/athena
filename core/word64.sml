structure Word64 : WORD =
struct

structure W32 = Word32

(* little endian format *)
type word = W32.word * W32.word

val wordSize = 64

(* assumes LargeWord is W32,
   as if LargeWord is W64 then this wont be used *)

fun toLarge (l,_) = l

fun toLargeX (l,_) = l

val toLargeWord = toLarge

val toLargeWordX = toLargeX

fun fromLarge lw : word = case W32.andb(lw, 0wx80000000) of
			      0w0 => (lw, 0w0)
			    | _ => (lw, 0wxffffffff)

fun fromLargeWord lw : word = (lw, 0w0)

fun toLargeInt (l,h) = 
    LargeInt.+ (W32.toLargeInt l,
		LargeInt.* (W32.toLargeInt h,
			    0x100000000))

fun toLargeIntX (l,h) =
    case W32.andb(h,0wx80000000) of
	0w0 => toLargeInt (l,h)
      | _ => (LargeInt.~ o LargeInt.+)
		 (toLargeInt (W32.notb l, W32.notb h),
		  1)

fun fromLargeInt i = (W32.fromLargeInt i, 
		      W32.fromLargeInt
			  (LargeInt.div (i,0x100000000)))

fun toInt w = LargeInt.toInt (toLargeInt w) 

fun toIntX w = LargeInt.toInt (toLargeIntX w)

fun fromInt i = fromLarge (W32.fromInt i)

fun andb ((l1,h1),(l2,h2)) = (W32.andb(l1,l2),
			      W32.andb(h1,h2))
fun orb ((l1,h1),(l2,h2)) = (W32.orb(l1,l2),
			     W32.orb(h1,h2))
fun xorb ((l1,h1),(l2,h2)) = (W32.xorb(l1,l2),
			      W32.xorb(h1,h2))
fun notb (l,h) = (W32.notb l, Word32.notb h)
fun << ((l,h),b) = 
    case Word.compare (b,0w32) of
	LESS => (W32.<<(l,b),
		 W32.orb(W32.<<(h,b),
			 W32.>>(l,Word.-(0w32,b))))
      | _ => (0w0,W32.<<(l,Word.-(b,0w32)))

fun >> ((l,h),b) = 
    case Word.compare (b,0w32) of
	LESS => (W32.orb(W32.>>(l,b),
			 W32.<<(h,Word.-(0w32,b))),
		 W32.>>(h,b))
      | _ => (W32.>>(h,Word.-(b,0w32)), 0w0)
fun ~>>((l,h),b) = 
    case Word.compare (b,0w32) of
	LESS => (W32.orb(W32.>>(l,b),
			 W32.<<(h,Word.-(0w32,b))),
		 W32.~>>(h,b))
      | _ => (W32.~>>(h,Word.-(b,0w32)),0wxFFFFFFFF)


fun (l1,h1) + (l2,h2) =
    let val l = W32.+(l1,l2)
    in case (W32.>=(l,l1),W32.>=(l,l2)) of
	   (true,true) => (l, W32.+(h1,h2))
	 | _ => (l,W32.+(h1,W32.+(h2,0w1))) end


fun (l1,h1) - (l2,h2) = 
    case W32.compare(l1,l2) of
	LESS => (W32.-(l1,l2), W32.-(W32.-(h1,0w1),h2))
	| _ => (W32.-(l1,l2), W32.-(h1,h2))

      
fun w1*w2 = raise Fail "NYI"
    
fun w1 div w2 = raise Fail "NYI"
fun w1 mod w2 = raise Fail "NYI"

fun compare ((l1,h1),(l2,h2)) =
    case (W32.compare(l1,l2),W32.compare(h1,h2)) of
	(EQUAL, EQUAL) => EQUAL
      | (cmp, EQUAL) => cmp
      | (_, cmp) => cmp

fun w1 < w2 = case compare(w1,w2) of 
		  LESS => true
		| _ => false
fun w1 <= w2 = case compare(w1,w2) of
		   GREATER => false
		 | _ => true
fun w1 > w2 = case compare(w1,w2) of
		  GREATER => true
		| _ => false
fun w1 >= w2 = case compare(w1,w2) of
		   LESS => false
		 | _ => true

fun ~ w = notb w + (0w1,0w0)
fun min (w1,w2) = if w1<w2 then w1 else w2
fun max (w1,w2) = if w1>w2 then w1 else w2

fun fmt rad w = IntInf.fmt rad (toLargeInt w)

fun pad s p 0 = s
  | pad s p n = pad (s^p) p (Int.-(n,1))

fun toString (l,h) = 
    case W32.compare(h,0w0) of
	GREATER =>
	let val sl = W32.toString l
	in pad (W32.toString h) "0"
	       (Int.-(8,size sl))
	   ^ sl end
      | _ => W32.toString l


fun scan rad c w = raise Fail "NYI"
fun fromString s = raise Fail "NYI"

end
