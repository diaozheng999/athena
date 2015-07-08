structure Heap : HEAP =
struct

open Core
	 
type 'a elem = int * Time.time * 'a
type 'a heap = int * int * 'a elem option array ref * Timer.cpu_timer
			      
fun empty () = (0, 8, ref (Array.array (8,NONE)), Timer.startCPUTimer ())
		   
fun isEmpty (0,_,_,_) = true
  | isEmpty _ = false
		    

fun swap (arr, i, j)=
    (print ("swapping len="^Int.toString (Array.length arr)^
	    " i="^Int.toString i^" j="^Int.toString j^"\n");
    let val (p, q) = (Array.sub (arr, i), Array.sub(arr,j))
    in (Array.update (arr, i, q); Array.update (arr, j, p)) end)




fun safeSub (arr, i) = Array.sub (arr, i) handle _ => NONE
	
fun compare (arr, i, j) =
    case (safeSub (arr, i), safeSub (arr, j)) of
	(NONE, NONE) => EQUAL
      | (NONE, SOME _) => GREATER
      | (SOME _, NONE) => LESS
      | (SOME (i,ii, _), SOME (j,jj, _)) =>
	(case Int.compare (i,j) of
	     EQUAL => Time.compare (ii, jj)
	   | ord => ord)     

fun push (size, cap, arr, timer) (priority, value) = 
    let
	val recap = ref cap
	val elem = (priority, #sys (Timer.checkCPUTimer timer), value)
    in
	( if size=cap then let val narr = Array.array (cap*2, NONE)
			   in (Array.copy {src=(!arr), dst=narr, di=0};
			       arr := narr;
			       recap := cap * 2) end else ();
	  Array.update (!arr, size, SOME elem);
	  let fun heapify 0 = ()
		| heapify n = case compare (!arr, n, (n-1) div 2) of
				  LESS => swap (!arr, n, (n-1) div 2)
				| _ => ()
	  in heapify size end;
	  (size+1, !recap, arr, timer))
    end
	
fun pop (heap as (size, cap, arr, timer) ) =
    case size of
	0 => (NONE, (size, cap, arr, timer))
      | _ => let
	  val SOME(_,_, res) = Array.sub (!arr, 0)
	  val recap = ref cap
	  fun heapify n =
	      let val left = (n*2)+1
		  val right = (n+1)*2
	      in
		  case (compare (!arr, n, left),
			compare (!arr, n, right),
			compare (!arr, left, right)) of
		      (GREATER, _, LESS) => (swap (!arr, n, left); heapify left)
		    | (_, GREATER, GREATER) => (swap (!arr, n, right); heapify right)
		    | _ => ()
	      end
						       
      in (Array.update (!arr, 0, NONE);
	  swap (!arr, 0, size-1);
	  heapify 0;
	  if max((size-1)*4, 8)< cap then let
	      val narr = Array.array (cap div 2, NONE)
	  in (ArraySlice.copy {src=ArraySlice.slice(!arr, 0, SOME  (size-1)),
			       dst=narr,
			       di=0};
	      arr := narr;
	      recap := cap div 2) end
	  else ();
	  (SOME res, ((size-1), !recap, arr, timer)))
      end


fun clone (a,b,arr,timer) = 
    let val narr = Array.array (Array.length (!arr), NONE)
    in (Array.copy {src=(!arr), dst=narr, di=0};
	(a,b,ref narr,timer)) end


fun toList (_,_,ref arr,_) =
    List.tabulate (Array.length arr, 
		   (fn i => case Array.sub (arr, i) of
				NONE => NONE
			      | SOME (_,_, v) => SOME v))
			

fun toArray l = Array.fromList (toList l) 

fun toVector l = Vector.fromList (toList l)


(* test cases *)


fun d__testCases param =
    case param of
	Env.DEBUG =>
	let
	    (* heap creation *) 
	    val heap : int heap = empty ()
	    
	    (* heap emptiness detection *)
	    val true = isEmpty heap
	    val heap = push heap (1, 0)
	    val false = isEmpty heap
	    val (_, heap) = pop heap
	    val true = isEmpty heap


	    (* heap pop detection *)
	    val list = List.tabulate (10, fn n => (n, n))
	    val heap = List.foldl (fn (elem, heap) => push heap elem) heap list 
	    val heap = List.foldl (fn (elem as (_, i), heap) => 
				      case pop heap of
					  (SOME e, heap2) => if e=i then heap2
							     else raise Fail ("Invalid element. Expect "^
									      (Int.toString i)^", got "^
									      (Int.toString e)^"\n")
					| _ => raise Fail ("Invalid element. Expect "^(Int.toString i)^
							   ", got rubbish.\n")) heap list

	in () end
      | _ => ()

val () = d__testCases Env.debug
end
