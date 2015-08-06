
type 'a task = 'a Task.task


signature ATHENA_TIMER =
sig

    type timer

    val addContinuousTrigger : timer 
			       -> (int -> unit task)
			       -> unit task

    val addValueTrigger : timer
			  -> int * (int -> unit task)
			  -> unit task

    val start : (int -> int task) -> timer * unit task

    val delay : int -> unit task

    val stop : timer -> unit task

    val stopAt : timer * int -> unit task

    val setInterval : int * unit task -> timer * unit task

    val setTimeout : int * unit task -> timer * unit task

end
