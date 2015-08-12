structure SHA256 : HASH_ALGO =
struct

open AthenaCore
open AthenaCore.WordCvt


structure VS = Word8VectorSlice

type digest = Word8Vector.vector
type message = VS.slice

val outputLength = 32
val algo : AthenaCore.Word8.word = 0wx2

fun psingleton s = 
    VectorSlice.full (Vector.tabulate (1, fn _ => s))

val tabulate = VS.full o Word8Vector.tabulate

local
    open AthenaCore.Word32
    infix andb orb xorb >>
in

fun Ch (x,y,z) = (x andb y) xorb (notb x andb z)
fun Maj (x,y,z) =
    (x andb y) xorb (x andb z) xorb (y andb z)

fun Sigma_0 x = rotr (x,0w2) xorb
		     rotr (x, 0w13) xorb
		     rotr (x, 0w22)

fun Sigma_1 x = rotr (x,0w6) xorb
		     rotr (x, 0w11) xorb
		     rotr (x, 0w25)

fun sigma_0 x = rotr (x,0w7) xorb
		     rotr (x, 0w18) xorb
		     (x >> 0w3)

fun sigma_1 x = rotr (x,0w17) xorb
		     rotr (x, 0w19) xorb
		     (x >> 0w10)

val K : word vector
    = Vector.fromList
	  [0wx428a2f98,0wx71374491,0wxb5c0fbcf,0wxe9b5dba5,
	   0wx3956c25b,0wx59f111f1,0wx923f82a4,0wxab1c5ed5,
	   0wxd807aa98,0wx12835b01,0wx243185be,0wx550c7dc3,
	   0wx72be5d74,0wx80deb1fe,0wx9bdc06a7,0wxc19bf174,
	   0wxe49b69c1,0wxefbe4786,0wx0fc19dc6,0wx240ca1cc,
	   0wx2de92c6f,0wx4a7484aa,0wx5cb0a9dc,0wx76f988da,
	   0wx983e5152,0wxa831c66d,0wxb00327c8,0wxbf597fc7,
	   0wxc6e00bf3,0wxd5a79147,0wx06ca6351,0wx14292967,
	   0wx27b70a85,0wx2e1b2138,0wx4d2c6dfc,0wx53380d13,
	   0wx650a7354,0wx766a0abb,0wx81c2c92e,0wx92722c85,
	   0wxa2bfe8a1,0wxa81a664b,0wxc24b8b70,0wxc76c51a3,
	   0wxd192e819,0wxd6990624,0wxf40e3585,0wx106aa070,
	   0wx19a4c116,0wx1e376c08,0wx2748774c,0wx34b0bcb5,
	   0wx391c0cb3,0wx4ed8aa4a,0wx5b9cca4f,0wx682e6ff3,
	   0wx748f82ee,0wx78a5636f,0wx84c87814,0wx8cc70208,
	   0wx90befffa,0wxa4506ceb,0wxbef9a3f7,0wxc67178f2]


end

fun pad m =
    let val len = VS.length m * 8
	val p = mono (w64to8b (psingleton (Word64.fromInt len)))
	val pad = tabulate ((448 - (len mod 512)) div 8,
			    (fn 0 => 0wx80
			    | _ => 0wx0))
							  
    in VS.full (VS.concat [m, pad, p]) end
    
fun chunkify m =
    Vector.tabulate (VS.length m div 64,
		     fn i => w8to32b (poly (VS.subslice (m, i*64, SOME 64))))

val H0:Word32.word vector = 
    Vector.fromList
	[0wx6a09e667,0wxbb67ae85,0wx3c6ef372,0wxa54ff53a,
	 0wx510e527f,0wx9b05688c,0wx1f83d9ab,0wx5be0cd19]

val preprocess = chunkify o pad

local
    structure A = Array
in

fun sha256 (M, H) =
    let val W = A.tabulate (64, fn _ => 0wxffffffff)
	val () = 
	    A.modifyi
		(fn (t,w) =>
		    case Int.compare (t,15) of
			GREATER =>
			sigma_1(A.sub(W,t-2)) +
			A.sub(W,t-7) +
			sigma_0(A.sub(W,t-15)) +
			A.sub(W,t-16)
		      | _ => VectorSlice.sub(M,t)) W
	fun hash' V 64 = V
	  | hash' (a,b,c,d,e,f,g,h) t =
	    let val T1 = h+Sigma_1(e)+Ch(e,f,g)+Vector.sub(K,t)
			 +A.sub(W,t)
		val T2 = Sigma_0(a)+Maj(a,b,c)
	    in hash' (T1+T2,a,b,c,d+T1,e,f,g) (t+1) end
	val (a,b,c,d,e,f,g,h)
	    = hash' (Vector.sub (H, 0),
		     Vector.sub (H, 1),
		     Vector.sub (H, 2),
		     Vector.sub (H, 3),
		     Vector.sub (H, 4),
		     Vector.sub (H, 5),
		     Vector.sub (H, 6),
		     Vector.sub (H, 7)) 0
    in Vector.tabulate (8, 
			(fn 0 => a + Vector.sub (H, 0)
			| 1 => b + Vector.sub (H, 1)
			| 2 => c + Vector.sub (H, 2)
			| 3 => d + Vector.sub (H, 3)
			| 4 => e + Vector.sub (H, 4)
			| 5 => f + Vector.sub (H, 5)
			| 6 => g + Vector.sub (H, 6)
			| _ => h + Vector.sub (H, 7)))
    end
	

fun hash m =
    (VS.vector o mono o w32to8b o VectorSlice.full)
	(Vector.foldl sha256 H0 (preprocess m))
	    
end


end
