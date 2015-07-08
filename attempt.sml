
fun fact 0 = 1
  | fact n = n * fact (n-1)





datatype 'a task = DO of unit -> 'a state
and 'a state = YIELD of 'a | RUN of 'a task

fun delay (f:unit -> 'a state) : 'a task = DO f

fun eval (DO f) : 'a state = f ()

fun yield (x:'a) : 'a task= DO (fn () => YIELD x)

fun suspend task = DO (fn () => RUN task)

fun await (task : 'a task, cont : 'a -> 'a task) : 'a task = 
    DO (fn () =>
	   case eval task of
	       YIELD value => RUN (cont value)
	     | RUN step => RUN (await (step,cont)))
			
infix 3 await	 


fun lazyfact 0 = yield 1
  | lazyfact n = (lazyfact (n-1))
		 await (fn result => yield (n*result))


fun run task =
    case eval task of
	YIELD value => value
      | RUN step => run step

