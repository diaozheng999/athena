type 'a seq = 'a AthenaData.Seq.seq
type 'a stream = 'a AthenaData.Stream.stream
type 'a task = 'a AthenaAsync.Task.task
type serialised = AthenaCore.Serialiser.serialised

signature HASH_ALGO =
sig
    type message = Word8VectorSlice.slice
    type digest = Word8Vector.vector
    val outputLength : int

    (* NOTE:
       type representation takes the lower 4 bits of
       a byte.
       0x0 - MD5
       0x1 - SHA1
       0x2 - SHA256
       0x3 - SHA224
       0x4 - SHA512
       0x5 - SHA384
       0x6 - SHA512/224
       0x7 - SHA512/256 *)

    val algo : AthenaCore.Serialiser.type_repr  
    val hash : message -> digest
end

signature HASH =
sig

type message = Word8VectorSlice.slice
type digest

val hash :  ('a -> serialised task)
	    -> 'a -> digest task

val validate : ('a -> serialised task)
	       -> ('a * digest) -> bool task

val toString : digest -> string task

val hashString : ('a -> serialised task) -> 'a -> string task

include ATHENA_CORE_ASYNC_SERIALISABLE

end
