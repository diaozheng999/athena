functor Hash (Algo : HASH_ALGO) : HASH =
struct

type message = Word8VectorSlice.slice
type digest = Word8Vector.vector
type t = digest

open AthenaCore
open AthenaAsync.Task
structure VS = Word8VectorSlice

infix <| <|| await
infixr |> ||>

local
    open AthenaCore.Word8
in
val kind = orb (<< (0wx3, 0wx4), Algo.algo)
end

fun hash s = s |> (async Serialiser.toBytes) |> (async Algo.hash)

fun validate s (a,h) =
    hash s a await (fn h' => yield (h = h'))

fun toString h =
    yield (Serialiser.toString "" (Serialiser.fromBytes (VS.full h)))

fun hashString f = hash f |> toString

local
    open Serialiser
    structure V = Word8Vector
    structure W = AthenaCore.Word8
in
val kind = W.orb (W.<< (0wx3, 0wx4), Algo.algo)
fun serialise h = 
    (yield o extern o VS.full o VS.concat)
	[(VS.full o V.fromList) [kind], VS.full h]

fun unserialise s =
    ensure (unpackExtern s) (Algo.outputLength + 1, kind)
	   (fn () =>
	       yield (VS.vector (VS.subslice (s,2,SOME Algo.outputLength)),
		      VS.subslice (s,Algo.outputLength+2, NONE)))
end

end
