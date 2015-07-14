structure Serialiser (*: SERIALISER*) =
struct

structure V = Word8Vector
structure VS = Word8VectorSlice
structure W32 = Word32
structure W8 = Word8
structure W = Word
open IEEEReal

type serialised = VS.slice
type type_expr = W8.word
datatype endian = LITTLE | BIG

exception Type

(* utility values *)
val bits : V.vector = V.fromList
                           [0wx80,0wx40,0wx20,0wx10,0wx08,0wx04,0wx02,0wx01]



(* utility functions *)

(* singleton : W8.word -> VS.slice *)
fun singleton s = VS.full (V.tabulate (1, fn _ => s))
(* concat : VS.slice * VS.slice -> VS.slice *)
fun concat (a,b) = VS.full (VS.concat [a,b])
(* tabulate : int * int -> W8.word -> VS.slice *)
val tabulate = VS.full o V.tabulate
(* mask : W32.word -> W32.word *)
fun mask w = W32.andb(w, 0wxFF)
(* toInt32 : W32.word -> int *)
fun toInt32 w = case W32.andb(w, 0wx80000000) of
                    0wx0 => W32.toInt w
                  | _ => ~(W32.toInt (W32.notb w))-1
(* toBytes : endian -> W32.word -> VS.slice *)
fun toBytes en w =
    let val conv = W8.fromInt o W32.toInt o mask
    in tabulate (4, fn i => case en of
                                LITTLE => conv (W32.>>(w, W.fromInt (i*8)))
                              | BIG => conv (W32.>>(w, W.fromInt ((3-i)*8))))
    end
(* fromBytes : endian -> VS.slice -> W32.word *)
fun fromBytes en w =
    let val conv = mask o W32.fromInt o W8.toInt
        fun shift (v,b) = W32.orb(W32.<<(b,0wx8),conv v)
    in case en of
           LITTLE => VS.foldr shift 0wx0 w
         | BIG => VS.foldl shift 0wx0 w
    end

fun normalise r =
    let val exp = Real.floor (Math.ln (Real.abs r)/Math.ln 2.0)
    in (Real.signBit r, exp, (Real.abs r) / Math.pow(2.0, Real.fromInt exp)-1.0)
    end

fun realToBitVector v bits =
    let fun toBitVector' v 0 = []
          | toBitVector' v n =
            let val v' = v * 2.0
            in case Real.compareReal (v', 1.0) of
                   LESS => false :: toBitVector' v' (n-1)
                   (* GREATER, EQUAL and UNORDERED all induce a true bit *)
                 | _ => true :: toBitVector' (v'-1.0) (n-1)
            end
    in Vector.fromList (toBitVector' v bits) end

fun expToBits exp bits =
    let val k = W32.<<(0w1, W.fromInt (bits-1))
        val repr = W32.xorb(W32.fromInt (exp-1), k)
    in Vector.tabulate (bits,
                        (fn i =>
                         W32.andb(repr, W32.>>(k, W.fromInt i)) <> 0w0)) end

fun toBitVector s =
    Vector.tabulate (VS.length s*8,
                     (fn i =>
                         let val (n,b) = (i div 8, i mod 8)
                         in case W8.andb(VS.sub(s,n), V.sub(bits,b)) of
                                0wx0 => false
                              | _ => true end))


val bvsToWord = VectorSlice.foldl (fn (false, acc) => W8.<<(acc, 0w1)
                                  | (true, acc) => W8.orb(W8.<<(acc,0w1),
                                                            0w1)) 0w0

fun fromBitVector bv =
    tabulate (Vector.length bv div 8,
              (fn i => bvsToWord (VectorSlice.slice (bv, i*8, SOME 8))))




(* external utility functions *)

fun getType s = VS.sub(s,0) handle Subscript => raise Type

fun toGeneric s = VS.subslice(s,1,NONE)

fun fromGeneric (s,t) = concat (singleton t, s)

fun isType s t = getType s = t

fun cast (s,t1,t2) =
    case isType s t1 of
        true => fromGeneric(toGeneric s, t2)
      | false => raise Type



(* packing functions *)
val packUnit = let val unit = singleton 0wx0 in fn () => unit end

val packBool = let val T = VS.full (V.fromList [0w1,0w1])
                   val F = VS.full (V.fromList [0w1,0w0])
               in (fn true => T
                  | false => F) end

fun packInt i = concat (singleton 0wx3, toBytes LITTLE (W32.fromInt i))

fun packFloat (exp,mts) r =
    case Real.class r of
        NAN _ => fromBitVector
                     (Vector.concat [Vector.fromList [Real.signBit r],
                                     Vector.tabulate (exp+mts, fn _ => true)])
      | INF => fromBitVector
                   (Vector.concat [Vector.fromList [Real.signBit r],
                                   Vector.tabulate (exp, fn _ => true),
                                   Vector.tabulate (mts, fn _ => false)])
      | ZERO => fromBitVector
                    (Vector.concat [Vector.fromList [Real.signBit r],
                                    Vector.tabulate (exp, fn _ => false),
                                    Vector.tabulate (mts, fn _ => false)])
      | NORMAL =>
        let val (s,e,m) = normalise r
        in fromBitVector (Vector.concat [Vector.fromList [s],
                                         expToBits e exp,
                                         realToBitVector m mts]) end
      | SUBNORMAL =>
        let val m = r / (Math.pow (2.0, 2.0 -
                                        Math.pow (2.0, Real.fromInt exp - 1.0)))
        in fromBitVector (Vector.concat [Vector.fromList [Real.signBit r],
                                         Vector.tabulate (exp, fn _ => false),
                                         realToBitVector m mts]) end

fun packReal r = concat (singleton 0wx04, packFloat (8, 23) r)

fun packReal64 r = concat (singleton 0wx10, packFloat (11, 52) r)

(* unpacking functions *)

fun minLength s len = VS.length s >= len

fun unpackUnit s =
    case minLength s 1 andalso isType s 0wx0 of
        true => ((), VS.subslice (s,1,NONE))
      | false => raise Type

fun unpackBool s =
    case minLength s 2 andalso isType s 0wx1 of
        false => raise Type
      | true => (case VS.sub(s,1) of
                     0w0 => false
                   | 0w1 => true
                   | _ => raise Type,
                 VS.subslice(s,2,NONE))

fun unpackInt s =
    case minLength s 5 andalso isType s 0wx3 of
        true => (toInt32 (fromBytes LITTLE (VS.subslice(s,1,SOME 4))),
                 VS.subslice(s,5,NONE))
      | false => raise Type



(* debug functions *)
fun wordToString w = let val s = W8.toString w
                     in case String.size s of
                            1 => "0"^s
                          | _ => s end

fun toString sep s = VS.foldr (fn (a,b) => wordToString a^sep^b) "" s

fun fromByteVector v = tabulate (Vector.length v, fn i => Vector.sub (v,i))

fun bitVectorToString b =
    Vector.foldr (op ^) "" (Vector.map (fn true => "1" | false => "0") b)

end
