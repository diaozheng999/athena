local
    open AthenaCore.Task
in

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

end


end
