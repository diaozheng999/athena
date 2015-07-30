structure Stream : STREAM =
struct
open AthenaCore
open AthenaAsync.Task
infix 3 await
infix 3 <| <||
infixr 3 |> ||>

type 'a pair = 'a Seq.pair
type 'a seq = 'a Seq.seq

datatype 'a stream = EOS | VAL of unit -> 'a * 'a stream

fun read EOS = yield NONE
  | read (VAL d) = yield (SOME (d ()))

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

end
