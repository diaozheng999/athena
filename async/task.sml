structure Task : TASK =
struct

open AthenaCore.TopLevel

datatype 'a task = Do of unit -> 'a state
and 'a state = Result of 'a | Run of 'a task


fun delay f = Do f

fun eval (Do f) = f ()

fun yield x = Do (fn () => Result x)

fun suspend t = Do (fn () => Run (t ()))

fun await (task, cont) =
    Do (fn () =>
	   case eval task of
	       Result v => Run (cont v)
	     | Run step => Run (await (step, cont)))

infix 3 await

fun run task =
    case eval task of
	Result v => v
      | Run step => run step


fun isComplete task =
    case eval task of
	Result _ => true
      | _ => false



fun join (t1 : 'a task, t2 : 'b task) : ('a * 'b) task =
    Do (fn () =>
	   case eval t1 of
	       Result v1 => Run (t2 await (fn v2 => yield (v1, v2)))
	     | Run step1 =>
	       Run (Do (fn () =>
			   case eval t2 of
			       Result v2 => Run (step1 await (fn v1 => yield (v1, v2)))
			     | Run step2 => Run (join (step1, step2)))))



fun weave (tasklist: 'a task Queue.queue, acc: 'a list) : 'a list task =
    Do (fn () =>
	   (case eval (Queue.dequeue tasklist) of
	       Result v => Run (weave (tasklist, v::acc))
	     | Run step => Run (Queue.enqueue (tasklist, step);
				weave (tasklist, acc)))
	   handle Queue.Dequeue => Run (yield acc))


fun concurrent (tasklist : 'a task vector) (i : 'a) : 'a vector task =
    let
	val q : (int * 'a) task Queue.queue = Queue.mkQueue ()
	val () = Vector.foldl (fn (t, ()) => Queue.enqueue (q,t)) ()
			      (Vector.mapi (fn (i, task) => task await (fn res => yield (i, res)))
					   tasklist)
    in
	weave (q, []) await
	    (fn reslist =>
		let val return = Array.array (Vector.length tasklist, i)
		in (List.app
			(fn (i,v) => Array.update (return, i, v))
			reslist;
		    yield (Array.vector return))
		end)
    end




fun ignore task = task await (fn _ => yield ())

fun async f x = Do (fn () => Result (f x))

fun <| (t1, t2) param = (t2 param) await (fn res => t1 res)

fun |> (t1, t2) param = (t1 param) await (fn res => t2 res)

fun ||> (t1, t2) = t1 await (fn () => t2)

fun <|| (t1, t2) = t2 await (fn () => t1)

fun select {cond=cond, true=ifT, false = ifF} = cond await (fn true => ifT
                                                             | false => ifF)

end
