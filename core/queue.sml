functor Queue (S:STACK) : QUEUE =
struct

type 'a queue = 'a S.stack * 'a S.stack

fun empty () = (S.empty (), S.empty ())

fun isEmpty (ins, outs) = S.isEmpty ins andalso S.isEmpty outs

fun flush (ins, outs) = if S.isEmpty ins then (ins, outs)
			else (S.empty (), S.rev' ins outs)

fun enq (q as (ins, outs)) a = (S.push ins a, outs)

fun deq (q as (ins, outs)) = if isEmpty q then (NONE, empty ())
	    else if S.isEmpty outs then deq (flush q)
	    else let
		val (e, outs) = S.pop outs
	    in (e, (ins, outs)) end

fun rev (ins, outs) = (S.rev outs, S.rev ins)

fun clone q = q

fun toList q = let
    val (_, s) = flush q
in S.toList s end

fun toArray q = Array.fromList (toList q)
fun toVector q = Vector.fromList (toList q)

fun d__testCases param = 
    case param of
	Env.DEBUG =>
	let 
	    val e = empty ()
	    val true = isEmpty e
	    val false = (isEmpty o (enq (empty ()))) 1
	    val (_, q) = deq (enq (empty ()) 1)
	    val true = isEmpty q
	in () end
      | _ => ()
		 
val () = d__testCases Env.debug

end

structure BaseQueue = Queue(Stack)
