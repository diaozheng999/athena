structure Timer : ATHENA_TIMER =
struct

open AthenaCore.TopLevel
open Task
open AthenaCore
open AthenaCore.Heap

structure HT = HashTable

infix <| <|| await
infixr |> ||>

exception Trigger

type timer = 
     { t : int ref,
       ct : (int -> unit task) vector ref,
       vt : (int * (int -> unit task)) heap ref,
       r : bool ref}

fun ct (t:timer) = !(#ct t)
fun vt (t:timer) = !(#vt t)
fun t (t':timer) = !(#t t')

fun addContinuousTrigger timer trigger =
    yield 
	(#ct timer := Vector.concat [Vector.fromList [trigger],
				     ct timer])

fun addValueTrigger timer (v,trig) =
    yield
	(#vt timer := push (vt timer) (v,(v,trig)))

fun genId () = UUID.toString UUID.UUID (UUID.generate ())

local
    open Timer
    open Time
in
fun delay mils =
    let fun sleep t =
	    async IntInf.compare
		  (toMilliseconds (checkRealTimer t),
		   IntInf.fromInt mils)
		  await (fn LESS => sleep t
			| _ => yield ())
    in (async startRealTimer |> sleep) () end
end

fun stop (timer:timer) = yield (#r timer := false)
    

fun start tickFn =
    let
	val timer = let
	    val ect : (int -> unit task) vector ref
		= ref (Vector.fromList [])
	    val evt : (int * (int -> unit task)) heap ref
		= ref (Heap.empty ())
	in {t=ref 0,ct=ect,vt=evt, r=ref true} end

	fun updateTimer i =
	    yield (#t timer := i)

	fun peel i acc =
	    case peek (vt timer) of
		NONE => acc
	      | SOME(v,t) =>
		case v<=i of
		    false => acc
		  | true => (#vt timer := cdr (pop (vt timer));
			     peel i (t i::acc))

	fun tick () =
	    (tickFn 
		 |> updateTimer
		 |> (fn () => 
			concurrent
			    (Vector.concat
				 [Vector.map
				      (fn tl => tl (t timer))
				      (ct timer),
				  Vector.fromList
				      (peel (t timer) []),
				  propagate ()]) ())
		 |> done
	    ) (t timer)
	and propagate () =
	    case !(#r timer) of
		true => Vector.fromList [tick ()]
	      | false => Vector.fromList []

    in (timer, tick ()) end
	    
fun interval ms i = delay ms await (fn () => yield (i+1))

fun stopAt (timer,ticks) =
    addValueTrigger timer (ticks, fn _ => stop timer)

fun setInterval (ms,f) =
    let val (timer, task) = start (interval ms)
    in (timer, 
	addContinuousTrigger timer (fn _ => f)
	||> task) end


fun setTimeout (ms,f) =
    let val (timer,task) = start (interval ms)
    in (timer,
	addValueTrigger timer (1, (fn _ => ignore (join (f, stop timer))))) end
end
