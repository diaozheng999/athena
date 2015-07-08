

structure Main =
struct


datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
		

fun run () =
    let
	val _ = print "Hello world!\n"
		      					     
	fun grow f t 0 = t
	  | grow f t n = grow f (Node (t,f (), t)) (n-1) 
			      

	fun iter 0 = 0
	  | iter n = n + iter (n-1)

	val itern =
	    let val n = ref 0
	    in fn () =>( n:=(!n+1); iter (!n) )end

	val fibTree : int tree = grow itern Empty 10000
	
	fun pushstack push t 0 = push t fibTree
	  | pushstack push t n = pushstack push (push t fibTree) (n-1)


	open Time

	val timer = Timer.startCPUTimer ()
	val stack = pushstack Stack.push (Stack.empty ()) 10000000
	val t = Timer.checkCPUTimer timer
	val p = Time.toReal (#sys t + #usr t)

	val timer = Timer.startCPUTimer ()
	val stack2 = pushstack Stack2.push (Stack2.empty ()) 10000000
	val t = Timer.checkCPUTimer timer
	val q = Time.toReal (#sys t + #usr t)
					
    in
	(p,q)
    end


val (fntime, lsttime) = run ()

end
