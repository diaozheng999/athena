type slice = Word8VectorSlice.slice
type 'a task = 'a AthenaAsync.Task.task

signature HASH =
sig

type digest

val hash : slice -> digest task

(*
val toString : digest -> string AthenaCore.Task.task

val hashString : Word8VectorSlice.slice 
		 -> string AthenaCore.Task.task
*)
end
