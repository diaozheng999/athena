signature TASK =
sig

    (* REQUIRES: each evaluation step of task is total. Task itself need not be total. *)
    type 'a task

    type 'a state

    (* delays the evaluation and wraps it in a task. *)
    val delay : (unit -> 'a state) -> 'a task

    (* evaluates the task for one step and exposes its state. *)
    val eval : 'a task -> 'a state

    (* represents a task that immediately returns the value. *)
    val yield : 'a -> 'a task

    (* delay evaluation of the current task for one step. *)
    val suspend : (unit -> 'a task) -> 'a task

    (* await (task, cont) takes a task and applies the continuation
       function (e.g.: cont a) once task evaluates to Result a.
       this is usually used as an infix operator. e.g.
       task1 await (fn result => task2) *)
    val await : 'a task * ('a -> 'b task) -> 'b task

    (* runs the task, and does not stop until it reaches a value.
       NOTE: this function is not total. *)
    val run : 'a task -> 'a

    (* joins the execution of two tasks, if either one is not completed,
       execute both simulteneously. This is similar to

       task1 await (fn r1 => task 2 await (fn r2 => yield (r1, r2)))

       however, unlike the example above, it executes task1 and task2
       concurrently if task1 and task2 are not completed.

       REQUIRES: task1 and task2 are both thread safe*)
    val join : 'a task * 'b task -> ('a * 'b) task
    
    val zip : ('a -> 'b task) * ('c -> 'd task) -> 'a * 'c
	      -> ('b * 'd) task

    (* joins the execution of many tasks of the same return type together.
       ensure that all elements of the vector are thread-safe as they will
       all be executed concurrently

       REQUIRES: all elements in tasklist are thread safe*)
    val concurrent : 'a task vector -> 'a  -> 'a vector task

    (* a shorthand to run multiple tasks in a list concurrently. 
       NOTE: c l ~= ignore (concurrent (Vector.toList l) ()) *)
    val c : unit task list -> unit task

    (* ignores the output of the task. Task will evaluate to Result () if total. *)
    val ignore : 'a task -> unit task

    (* similar to ignore. usage: 
       (task1 |> task2 |> task3 |> done) (param)
    *)
    val done : 'a -> unit task

    (* check if the task is complete. This function is total given task invariant. *)
    val isComplete : 'a task -> bool

    (* runs a synchronous operation asynchronously.
       NOTE: the operation is run in one execution step. *)
    val async : ('a -> 'b) -> 'a -> 'b task

    (* runs an asynchronous operation synchronously.
       i.e. sync f x ~= run (f x) *)
    val sync : ('a -> 'b task) -> 'a -> 'b

    val <| : ('a -> 'b task) * ('c -> 'a task) -> 'c -> 'b task

    val |> : ('a -> 'b task) * ('b -> 'c task) -> 'a -> 'c task

    val ||> : unit task * unit task -> unit task

    val <|| : unit task * unit task -> unit task

    val select : {cond : bool task, true : 'b task, false : 'b task}
                 -> 'b task
end

(*
signature THREAD =
sig

    structure Q : QUEUE

    type 'a task

    type 'a message

    type thread

    datatype status = SLEEPING | BLOCKING | EXECUTING | DONE

    type state

    val start : 'a task -> 'a thread



end
*)
