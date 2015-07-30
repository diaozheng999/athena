structure SHA256 : HASH =
struct

type digest = Word8Vector.vector

local
    open AthenaCore.Word32
    infix andb orb xorb >>
in

fun Ch (x,y,z) = (x andb y) xorb (notb x andb z)
fun Maj (x,y,z) =
    (x andb y) xorb (x andb z) xorb (y andb z)

fun Sigma_0 x = rotr (x,2) xorb
		     rotr (x, 13) xorb
		     rotr (x, 22)

fun Sigma_1 x = rotr (x,6) xorb
		     rotr (x, 11) xorb
		     rotr (x, 25)

fun sigma_0 x = rotr (x,7) xorb
		     rotr (x, 18) xorb
		     (x >> 3)

fun sigma_1 x = rotr (x,17) xorb
		     rotr (x, 19) xorb
		     (x >> 10)

end

end
