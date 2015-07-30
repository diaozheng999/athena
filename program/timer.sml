structure Timer : ATHENA_TIMER =
struct

open AthenaCore
open AthenaCore.TopLevel
open AthenaCore.Task
open AthenaCore.Heap
open AthenaCore.Serialiser
open AthenaData.Event
open AthenaData.Seq


infix <| <|| await
infixr |> ||>


type timer =
     {t : int ref,
      id : string,
      ct : int vector ref,
      vt : (int * listener) heap ref,
      tick : int option ref,
      clr : (string * int vector) option ref}

val getInt = async (car o unpackInt)

fun addContinuousTrigger (t:timer) trig =
    ((!addListener) ("__ct-"^ #id t) 
     |> async (fn i =>
		  (#ct t) := 
		  Vector.concat [Vector.fromList [i],
				 !(#ct t)]))
	(getInt |> trig)
		   

fun addValueTrigger (t:timer) (v,trig) =
    yield ((#vt t) := push (!(#vt t)) (v,(v,getInt |> trig)))

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

	    
fun start tickFn =
    let 
	(* initialise timer object *)
	val timer = let
	    val ect : int vector ref 
		= ref (Vector.fromList [])
	    val evt : (int * listener) heap ref
		= ref (Heap.empty ())
	in {t=ref 0, 
	    ct=ect, 
	    vt=evt,
	    id=genId (),
	    tick=ref NONE,
	    clr=ref NONE
	   } end

	fun updateTimer i = 
	    (print ("Athena.Program.Timer._updateTimer: "^
		    Int.toString i^"\n");
	    if (i=(!(#t timer))) then raise Fail "invalid update"
	    else
		yield ((#t timer) := i))

	fun peel i = 
	    case peek (!(#vt timer)) of
		NONE => NONE
	      | SOME (v,t) =>
		case v<=i of
		    false => NONE
		  | true => (#vt timer := cdr (pop (!(#vt timer)));
			     SOME (v,t))


	fun clrTimeBase () =
	    case !(#clr timer) of
		NONE => yield ()
	      | SOME (s,l) =>
		concurrent 
		    (Vector.map (!removeListener s) l) ()
		    await
		    (fn _ => yield (#clr timer := NONE))

	fun timeBasedComparison i =
	    case (peel i, !(#clr timer)) of
		(NONE,NONE) => yield ()
	      | (NONE,SOME(s,l)) => 
		!raiseEvent (s, packInt i)
	      | (SOME(v,t), clr) =>
		(case clr of
		     SOME(s,l) => yield ()
		   | NONE => 
		     yield (#clr timer := SOME ("__vt-"^genId (),
						Vector.fromList [])))
		    ||>
		    let val (s,l) = Option.valOf (!(#clr timer))
		    in (!addListener s t) 
			   await
			   (fn lid => 
			       yield (
				   #clr timer :=
				   SOME (s,
					 Vector.concat 
					     [l,
					      Vector.fromList [lid]]))
			   ) end
		    ||> timeBasedComparison i
	
	

	(* create the tick function *)
	fun tick _ =
	    (print ("Athena.Program.Timer: tick "^
		   Int.toString (!(#t timer))^"\n");
	    clrTimeBase () 
		||> (tickFn |> updateTimer) (!(#t timer))
		||> ignore
		(concurrent
		     (Vector.fromList
			  [(delay 200 ||> !raiseEvent
				("timer-"^(#id timer),
				 packUnit ())),
			   (!raiseEvent 
				("__ct-"^(#id timer),
				 packInt (!(#t timer)))),
			   (timeBasedComparison (!(#t timer)))])
		     ()))

    in (timer, 
	(!addListener ("timer-"^(#id timer)) tick
		      await
		      (fn id => yield (#tick timer := SOME id)
				      ||> !raiseEvent
				      ("timer-"^(#id timer),
				       packUnit ())))
       ) end

end
