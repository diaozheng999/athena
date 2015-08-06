structure Stream : STREAM =
struct
open AthenaCore
open AthenaAsync
open AthenaAsync.Task
infix 3 await
infix 3 <| <||
infixr 3 |> ||>

type 'a pair = 'a Seq.pair
type 'a seq = 'a Seq.seq

datatype 'a stream = EOS | VAL of unit -> 'a * 'a stream

fun read EOS = yield NONE
  | read (VAL d) = yield (SOME (d ()))

fun readN 0 s = Seq.empty () await (fn x => yield (x,s))
  | readN n s = 
    read s await
	 (fn NONE => readN 0 s
	 | SOME (x,xs) => (readN (n-1) |>
				 Task.zip (Seq.cons x, yield))
			      xs)

fun cons v s = yield (VAL (fn ()=>(v,s)))

fun append (a,b) = read a
                        await (fn NONE => yield b
                                | SOME (v,s) =>
                                  append (s,b)
                                         await (fn s =>
                                                   yield (VAL (fn () =>
                                                                  (v,s)))))


fun empty () = yield EOS

fun singleton a = yield (VAL (fn () =>(a,EOS)))

fun isEmpty EOS = yield true
  | isEmpty _ = yield false

fun ::: (a,b) = cons a <| b
infixr :::

fun fromSeq s = select {cond= Seq.isEmpty s,
                        true= empty (),
                        false= Seq.mapreduce1 singleton append s}

fun toList s = read s await (fn NONE => yield []
                              | SOME(e,s') => toList s await
                                                     (fn r => yield (e::r)))

fun toSeq s = (toList |> Seq.fromList) s

fun map f = read |> (fn NONE => yield EOS
                      | SOME(e,r) => f e await (fn e' => (e':::map f) r))

type 'a t = 'a stream

local
    open AthenaCore.Serialiser
    structure VS = Word8VectorSlice
    structure V = Word8Vector
    val fromList = VS.full o V.fromList
    val tabulate = VS.full o V.tabulate
    val concat = VS.full o VS.concat
in

fun serialise t s =
    read s await
	 (fn NONE => yield (fromList [0wxfe,0wx01,0wx00])
	 | SOME (x,xs) => 
	   join (serialise t xs, t x)
		await (fn (r,s) =>
			  yield (concat [fromList [0wxfe,0wx01,0wx01],
					 s,r])))

fun unserialise t s =
    ensure s (2,0wxfe)
	   (fn () =>
	       ensure (VS.subslice(s,1,NONE)) (2,0w1)
		      (fn () =>
			  case VS.sub(s,2) of
			      0w0 => yield (EOS, VS.subslice(s,3,NONE))
			    | 0w1 => t s
				       await
				       (fn (y,ys) =>
					   unserialise 
					       t ys 
					       await
					       (fn (r,rs) => join (cons y r, yield rs)))
			    | _ => raise Type))
end
						
end
