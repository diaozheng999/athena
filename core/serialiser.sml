structure Serialiser : SERIALISER =
struct

structure V = Word8Vector
structure VS = Word8VectorSlice
structure W32 = Word32
structure W8 = Word8
structure W = Word

open Core

type serialised = VS.slice
type type_repr = W8.word

datatype type_id = GENERIC
                     | INT
                     | LARGE_INT
                     | BOOL
                     | UNIT
                     | CHAR
                     | STRING
                     | REAL
                     | UTF_CHAR
                     | UTF_STRING
                     | ORDER of type_id
                     | TREE of type_id
                     | VECTOR of type_id
                     | ARRAY of type_id
                     | SERIALISED
                     | EXT


datatype endian = LITTLE | BIG

exception Type

(* utility values *)
val bits : V.vector = V.fromList
                           [0wx80,0wx40,0wx20,0wx10,0wx08,0wx04,0wx02,0wx01]

val null : VS.slice = VS.full (V.fromList [])

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

fun bytesToInt32 en = toInt32 o (fromBytes en)

fun normalise r =
    let val exp = Real.floor (Math.ln (Real.abs r)/Math.ln 2.0)
    in (Real.signBit r, exp, (Real.abs r) / Math.pow(2.0, Real.fromInt exp)-1.0)
    end

fun subnormal bits =
    (Math.pow (2.0, 2.0 - Math.pow (2.0, Real.fromInt bits - 1.0)))

local
  open IEEEReal
in

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

fun realFromBitVector v  =
    Vector.foldr (fn (true, x) => x / 2.0 + 1.0
                 | (false, x) => x / 2.0) 0.0 v / 2.0

fun expToBits exp bits =
    let val k = W32.<<(0w1, W.fromInt (bits-1))
        val repr = W32.xorb(W32.fromInt (exp-1), k)
    in Vector.tabulate (bits,
                        (fn i =>
                         W32.andb(repr, W32.>>(k, W.fromInt i)) <> 0w0)) end

fun expFromBits bits =
    let val k = toInt32(W32.<<(0w1, W.fromInt(Vector.length bits-1)))-1
        val repr = Vector.foldl (fn (true, x) => W32.orb(W32.<<(x,0w1),0w1)
                                  | (false,x) => W32.<<(x,0w1)) 0w0 bits
    in toInt32 repr - k end

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

end


(* external utility functions *)

fun getType s = VS.sub(s,0) handle Subscript => raise Type

fun isType s t = getType s = t

fun cast (s,t1,t2) =
    case isType s t1 of
        true => concat (singleton t2, VS.subslice(s,1,NONE))
      | false => raise Type

fun toGeneric s = cast (s,getType s,0wxff)

fun fromGeneric (s,t) = cast(s,0wxff,t)

fun interpret (s,t) = concat(singleton t, s)



(* packing functions *)
val packUnit = let val unit = singleton 0wx0 in fn () => unit end

val packBool = let val T = VS.full (V.fromList [0w1,0w1])
                   val F = VS.full (V.fromList [0w1,0w0])
               in (fn true => T
                  | false => F) end

fun packChar c = tabulate (2, (fn 0 => 0wx2
                              | _ => Byte.charToByte c))

fun packInt i = concat (singleton 0wx3, toBytes LITTLE (W32.fromInt i))

local
  open IEEEReal
in
fun packFloat (exp,mts) r =
    case (Real.class r, Real.isNan r) of
        (_,true) => fromBitVector
                        (Vector.concat [Vector.fromList [Real.signBit r],
                                        Vector.tabulate (exp+mts,fn _ => true)])
      | (INF,_) => fromBitVector
                       (Vector.concat [Vector.fromList [Real.signBit r],
                                       Vector.tabulate (exp, fn _ => true),
                                       Vector.tabulate (mts, fn _ => false)])
      | (ZERO,_) => fromBitVector
                        (Vector.concat [Vector.fromList [Real.signBit r],
                                        Vector.tabulate (exp, fn _ => false),
                                        Vector.tabulate (mts, fn _ => false)])
      | (NORMAL,_) =>
        let val (s,e,m) = normalise r
        in fromBitVector (Vector.concat [Vector.fromList [s],
                                         expToBits e exp,
                                         realToBitVector m mts]) end
      | (SUBNORMAL,_) =>
        let val m = r / subnormal exp
        in fromBitVector (Vector.concat [Vector.fromList [Real.signBit r],
                                         Vector.tabulate (exp, fn _ => false),
                                         realToBitVector m mts]) end

      (* this case should never be evaluated since for all reals, if isNan
         returns true, then Real.class should return NAN of something, hence
         the rest of the cases are exhaustive. However, since the compiler
         does not know this, the following case is written to override compile
         errors. *)
      | (_,_) => raise Fail "Unexpected evaluation of reals."
end

fun packReal r = concat (singleton 0wx04, packFloat (8, 23) r)

fun packReal64 r = concat (singleton 0wx10, packFloat (11, 52) r)


fun packUnsafeString s = let val len = size s
                         in concat (toBytes LITTLE (W32.fromInt len),
                                    VS.full (Byte.stringToBytes s)) end

fun packString s = concat (singleton 0wx05,
                           packUnsafeString s)

fun packLargeInt i =
    let
      (* record largeInts in big-endian because we can approximate values
         if bits gets chopped off, and we will not be orders of magnitudes off
       *)
      fun toList (0:IntInf.int) acc : W8.word list * W32.word= acc
        | toList n (al,ln) = toList
                                 (IntInf.div (n, 256))
                                 (W8.fromLargeInt (IntInf.mod(n, 256))::al,
                                  ln+0w1)

      val (bits, l) = toList (IntInf.abs i) ([],0w0)
      val len = case IntInf.< (i, 0) of
                    true => ~l
                  | _ => l
    in concat(concat(singleton 0w6, toBytes LITTLE len),
              VS.full (V.fromList bits)) end


val packOrder = let
  val less = tabulate (2, (fn 0 => 0wx0A | _ => 0wxFF))
  val equal = tabulate (2, (fn 0 => 0wx0A | _ => 0wx00))
  val greater = tabulate (2, (fn 0 => 0wx0A | _ => 0wx01))
in fn LESS => less | EQUAL => equal | GREATER => greater end

fun packWord w = concat (singleton 0wxC, toBytes LITTLE w)

fun packUChar c =
    (* we can save 1 byte because valid unicode code-points are
       from U+0000 - U+10ffff *)
    concat (singleton 0wxD,
            VS.subslice(toBytes LITTLE (W32.fromInt (Utf8Char.ord c)),
                        0,SOME 3))


fun packUString s = concat (singleton 0wxE,
                            packUnsafeString (Utf8String.toString s))

(* polymorphic packing functions *)

fun packList fs [] = tabulate (2, fn 0 => 0wx7 | _ => 0wx00)
  | packList fs (x::xs) = concat (tabulate (2, fn 0 => 0wx7 | _ => 0wx1),
                                  concat(fs x,packList fs xs))

fun packVector fs v =
    let val size = W32.fromInt (Vector.length v)
        val serialised = Vector.map fs v
        val (ptr,_) = Vector.foldl (fn (s,(chr,offs)) =>
                                       let val len = W32.fromInt (VS.length s)
                                       in (concat (chr, toBytes LITTLE offs),
                                           offs+len) end)
                                   (toBytes LITTLE size,0w4+0w4*size) serialised
        val body = concat (ptr,  Vector.foldr concat null serialised)
        val header = concat (singleton 0wx8,
                             toBytes LITTLE (W32.fromInt (VS.length body)))
    in concat(header, body) end

fun packArray fs a = cast (packVector fs (Array.vector a), 0w8, 0w9)

fun packOption _ NONE = tabulate(2, fn 0 => 0wxb | _ => 0wx00)
  | packOption fs (SOME v) = concat (tabulate (2, fn 0 => 0wxb | _ => 0wx1),
                                     fs v)

local
open Tree
in
fun packTree _ Empty = tabulate (2, fn 0 => 0wxf | _ => 0wx0)
  | packTree fs (Node(l,x,r)) =
    let val sx = fs x
        val left = packTree fs l
        val offset = 0w5+W32.fromInt (VS.length sx+VS.length left)
    in VS.full (VS.concat [singleton 0wxf, toBytes LITTLE offset,
                           sx, left, packTree fs r]) end
end





(* unpacking functions *)

fun minLength s len = VS.length s >= len

fun ensure s (l,t) k =
    case minLength s l andalso isType s t of
      true => k ()
    | false => raise Type

fun unpackUnit s =
    ensure s (1, 0w0) (fn () =>((), VS.subslice (s,1,NONE)))

fun unpackBool s =
    ensure s (2,0w1) (fn () => (case VS.sub(s,1) of
                                  0w0 => false
                                | 0w1 => true
                                | _ => raise Type,
                                VS.subslice(s,2,NONE)))

fun unpackChar s =
    ensure s (2, 0w2) (fn () => (Byte.byteToChar (VS.sub(s,1)),
                                 VS.subslice(s,2,NONE)))

fun unpackInt s =
    ensure s (5, 0w3) (fn () => (bytesToInt32 LITTLE (VS.subslice(s,1,SOME 4)),
                                 VS.subslice(s,5,NONE)))

fun unpackFloat (exp, mts) s =
    let val bits = toBitVector s
        val sign = Vector.sub(bits, 0)
        val expbits = VectorSlice.vector (VectorSlice.slice(bits, 1, SOME exp))
        val mtsbits = VectorSlice.vector (VectorSlice.slice(bits, 1+exp,
                                                            SOME mts))
        datatype allOrNone = ALL | NONE | NEITHER

        fun check bits = if Vector.all (fn x => x) bits then ALL
                         else if Vector.all not bits then NONE
                         else NEITHER
        val r = case (check expbits, check mtsbits) of
                    (ALL, NONE) => Real.posInf
                  | (ALL, _) => Math.sqrt (~1.0) (* NaN *)
                  | (NONE, NONE) => 0.0
                  | (NONE, _) => realFromBitVector mtsbits * subnormal exp
                  | _ => (1.0 + realFromBitVector mtsbits) *
                         Math.pow(2.0, Real.fromInt (expFromBits expbits))
    in case sign of
           true => r * ~1.0
         | false => r
    end

fun unpackReal s =
    ensure s (5,0w4) (fn () => (unpackFloat (8, 23) (VS.subslice(s,1,SOME 4)),
                                 VS.subslice(s,5,NONE)))

fun unpackReal64 s =
    ensure s (9,0wx10)
            (fn () => (unpackFloat (11, 52) (VS.subslice(s,1,SOME 8)),
                       VS.subslice(s,9,NONE)))

fun unpackUnsafeString s =
    case minLength s 4 of
        true =>
        let val len = bytesToInt32 LITTLE (VS.subslice(s,0,SOME 4))
        in (Byte.unpackStringVec(VS.subslice(s,4,SOME len)),
            VS.subslice(s,4+len,NONE)) end
      | false => raise Type

fun unpackString s =
    ensure s (1,0w5) (fn () => unpackUnsafeString (VS.subslice(s,1,NONE)))

fun unpackLargeInt s =
    ensure s (5,0w6)
            (fn () => let
                  val len = bytesToInt32 LITTLE (VS.subslice(s,1,SOME 4))
                  fun build len = VS.foldl (fn (b,x) =>
                                               IntInf.+(IntInf.*(x,256),
                                                        W8.toLargeInt b))
                                           0
                                           (VS.subslice(s,5,SOME len))
                in case len<0 of
                     true => (~(build (~len)),
                              VS.subslice(s,5-len,NONE))
                   | false =>(build len, VS.subslice(s, 5+len, NONE))
                end)

fun unpackOrder s =
    ensure s (2,0wxa)
            (fn () => case VS.sub(s,1) of
                        0wxFF => (LESS, VS.subslice(s,2,NONE))
                      | 0wx00 => (EQUAL, VS.subslice(s,2,NONE))
                      | 0wx01 => (GREATER, VS.subslice(s,2,NONE))
                      | _ => raise Type)


fun unpackWord s =
    ensure s (2,0wxc) (fn () => (fromBytes LITTLE (VS.subslice(s,1,SOME 4)),
                                  VS.subslice(s,5,NONE)))


fun unpackUChar s =
    ensure s (4, 0wxd)
            (fn () => (Utf8Char.chr
                           (bytesToInt32 LITTLE
                                         (concat (VS.subslice(s,1,SOME 3),
                                                  singleton 0w0))),
                       VS.subslice(s,4,NONE)))


fun unpackUString s =
    ensure s (1, 0wxe)
           (fn () => let val (s,cont) = unpackUnsafeString
                                            (VS.subslice(s,1,NONE))
                     in case Utf8String.fromString s of
                          SOME str => (str, cont)
                        | _ => raise Type end)

fun getListElem us s =
    ensure s (2, 0wx7)
           (fn () => case VS.sub(s,1) of
                       0w0 => (NONE, VS.subslice(s,2,NONE))
                     | 0w1 => let val (elem, r) = us (VS.subslice(s,2,NONE))
                              in (SOME elem, r) end
                     | _ => raise Type)

fun unpackList us s =
    case getListElem us s of
      (NONE, r) => ([], r)
    | (SOME x, r) => let val (rest, r') = unpackList us r
                     in (x::rest, r') end

fun getVectorSizeUnsafe v =
    case minLength v 4 of
      false => raise Type
    | true => bytesToInt32 LITTLE (VS.subslice(v,0,SOME 4))


fun getVectorElemUnsafe us (v,i) =
    let val offs = bytesToInt32 LITTLE (VS.subslice(v,(i+1)*4,SOME 4))
    in car (us (VS.subslice(v,offs,NONE))) end


fun getVectorSize v =
    ensure v (9, 0w8) (fn () => getVectorSizeUnsafe (VS.subslice(v,5,NONE)))

fun getVectorElem us (v,i) =
    ensure v (9, 0w8)
           (fn () => getVectorElemUnsafe us (VS.subslice(v,5,NONE), i))

fun unpackVector us v =
    ensure v (5, 0w8)
           (fn () =>
               let val roffs = bytesToInt32 LITTLE (VS.subslice(v,1,SOME 4))
               in (Vector.tabulate (getVectorSize v,
                                    (fn i => getVectorElem us (v,i))),
                   VS.subslice(v,5+roffs, NONE)) end)

fun getArraySize a =
    ensure a (9,0w9) (fn () => getVectorSizeUnsafe (VS.subslice(a,5,NONE)))

fun getArrayElem us (a,i) =
    ensure a (9,0w9)
           (fn () => getVectorElemUnsafe us (VS.subslice(a,5,NONE), i))

fun unpackArray us a =
    ensure a (9, 0w9)
           (fn () =>
               let val roffs = bytesToInt32 LITTLE (VS.subslice(a,1,SOME 4))
               in (Array.tabulate (getArraySize a,
                                   (fn i => getArrayElem us (a,i))),
                   VS.subslice(a,5+roffs,NONE)) end)

fun getOptionIsSome s = ensure s (2,0wxb) (fn () => 0w1 = VS.sub (s, 1))

fun unpackOption us s =
    case getOptionIsSome s of
      false => (NONE, VS.subslice(s,2,NONE))
    | true => let val (v,r) = us (VS.subslice(s,2,NONE))
              in (SOME v, r) end

local
  open Tree
in

fun getTreeNode us s =
    ensure s (2,0wxf)
           (fn () =>
               case VS.sub(s,1) of
                 0w0 => {value = NONE,
                         left = null,
                         right = VS.subslice(s,2,NONE)}
               | _ =>
                 (let val roffs = bytesToInt32 LITTLE
                                               (VS.subslice(s,1,SOME 4))
                      val (l,r) = us (VS.subslice (s,5,NONE))
                  in {value = SOME l,
                      left = r,
                      right = VS.subslice(s,roffs,NONE)} end)
                 handle Subscript => raise Type)

fun unpackTree us t =
    case getTreeNode us t of
      {value=NONE, left=_,right=r} => (Empty, r)
    | {value=SOME v,left=sl,right=sr} =>
      let val (r,rr) = unpackTree us sr
      in (Node(car (unpackTree us sl), v, r), rr) end



end

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



signature SERIALISABLE =
sig
    type t

    val serialise : t -> Serialiser.serialised
    val unserialise : Serialiser.serialised -> t
    val size : Serialiser.serialised -> int

end
