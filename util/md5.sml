structure MD5 : HASH =
struct

open AthenaCore
open AthenaCore.Task
open AthenaCore.Serialiser
open Word32
infix orb xorb andb to << >>


type hash = Word8Vector.vector

fun F (B,C,D) = (B andb C) orb (notb B andb D)
fun G (B,C,D) = (B andb D) orb (C andb notb D)
fun H (B,C,D) = B xorb C xorb D
fun I (B,C,D) = C xorb (B orb notb D)

val K : word vector = Vector.fromList [0wxd76aa478, 0wxe8c7b756, 0wx242070db, 0wxc1bdceee,
				       0wxf57c0faf, 0wx4787c62a, 0wxa8304613, 0wxfd469501,
				       0wx698098d8, 0wx8b44f7af, 0wxffff5bb1, 0wx895cd7be,
				       0wx6b901122, 0wxfd987193, 0wxa679438e, 0wx49b40821,
				       0wxf61e2562, 0wxc040b340, 0wx265e5a51, 0wxe9b6c7aa,
				       0wxd62f105d, 0wx02441453, 0wxd8a1e681, 0wxe7d3fbc8,
				       0wx21e1cde6, 0wxc33707d6, 0wxf4d50d87, 0wx455a14ed,
				       0wxa9e3e905, 0wxfcefa3f8, 0wx676f02d9, 0wx8d2a4c8a,
				       0wxfffa3942, 0wx8771f681, 0wx6d9d6122, 0wxfde5380c,
				       0wxa4beea44, 0wx4bdecfa9, 0wxf6bb4b60, 0wxbebfbc70,
				       0wx289b7ec6, 0wxeaa127fa, 0wxd4ef3085, 0wx04881d05,
				       0wxd9d4d039, 0wxe6db99e5, 0wx1fa27cf8, 0wxc4ac5665,
				       0wxf4292244, 0wx432aff97, 0wxab9423a7, 0wxfc93a039,
				       0wx655b59c3, 0wx8f0ccc92, 0wxffeff47d, 0wx85845dd1,
				       0wx6fa87e4f, 0wxfe2ce6e0, 0wxa3014314, 0wx4e0811a1,
				       0wxf7537e82, 0wxbd3af235, 0wx2ad7d2bb, 0wxeb86d391]

val (st as (a0:word, b0:word, c0:word, d0:word)) = (0wx67452301, 0wxefcdab89, 0wx98badcfe, 0wx10325476)

val s = Vector.map Word.fromInt
		   (Vector.fromList [7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 
				     5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 
				     4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 
				     6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21])
				    

fun leftrotate (x, c) = let val w32 : Word.word = 0w32 in (x << c) orb (x >> ( Word.-(w32, c))) end

fun hash' value =  
    let open Int
	val valueb = toBitVector value
	val len = Vector.length valueb
	val n = let val p = 447 -  (len mod 512) in
		    case Int.compare (p, 0) of
			LESS => 512 - p
		      | _ => p end
	val valueb = Vector.concat [valueb, Vector.fromList [true], Vector.tabulate (n, fn _ => false)]
	val chunks = Vector.tabulate 
			 (Vector.length valueb div 512,
			  fn i =>
			     Vector.tabulate 
				 (16, fn j =>
					 VectorSlice.foldl
					     (fn (b,w) => let val w = w << 0w1
							  in case b of true => w orb 0w1
								     | false => w end) 0w0
					     (VectorSlice.slice(valueb, i * 512 + j* 32, SOME 32))
				 )
			 )

	fun getFg i = case i div 16 of
			  0 => (F, i)
			| 1 => (G, (5 * i + 1) mod 16)
			| 2 => (H, (3 * i + 5) mod 16)
			| _ => (I, (7 * i) mod 16)

	open Word32
	val digest = Vector.foldl
			 (fn (M,(a0,b0,c0,d0)) => 
			     let val (A,B,C,D) = 
				     Vector.foldl
					 (fn (i,(A,B,C,D)) =>
					     let val (F,g) = getFg i
					     in (D,B+leftrotate(A+F(B,C,D)+Vector.sub(K,i)+Vector.sub(M,i),
								Vector.sub(s,i)),B,C) end)
					 (a0,b0,c0,d0) (0 to 63)
			     in (a0+A,b0+B,c0+C,d0+D) end)
			 (0wx67452301,0wxefcdab89,0wx98badcfe,0wx10325476) chunks
	val (d1,d2,d3,d4) = digest
	val cast = Word8.fromLargeInt o toLargeInt
    in Word8Vector.tabulate 
	   (16, fn 0 => cast d1 | 1 => cast (d1 >> 0w8) | 2 => cast (d1 >> 0w16) | 3 => cast (d1 >> 0w24)
		|  4 => cast d2 | 5 => cast (d2 >> 0w8) | 6 => cast (d2 >> 0w16) | 7 => cast (d2 >> 0w24)
		|  8 => cast d3 | 9 => cast (d3 >> 0w8) | 10 => cast (d3 >> 0w16) | 11 => cast (d3 >> 0w24)
		| 12 => cast d4 | 13 => cast (d4 >> 0w8) | 14 => cast (d4 >> 0w16) | 15 => cast (d4 >> 0w16)
		| _ => 0w0) end
			     

fun hash value = async hash' value

end
