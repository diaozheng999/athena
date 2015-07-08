



signature PROGRAM = sig

    structure Dict : sig
		  type 'a key
		  type 'a entry
		  type 'a dict
		  val add : 'a key * 'a entry -> 'a dict
		  val find : 'a dict -> 'a key -> 'a entry option
		  val remove : 'a dict -> 'a key -> 'a dict
	      end

    structure Queue : sig
		  type 'a queue
		  val empty : unit -> 'a queue
		  val isEmpty : 'a queue -> bool
		  val enq : 'a queue -> 'a -> 'a queue
		  val deq : 'a queue -> 'a -> 'a queue
	      end

    structure Stack : sig
		  type 'a stack
		  val empty : unit -> 'a stack
		  val isEmpty : 'a stack -> bool
		  val push : 'a stack -> 'a -> 'a stack
		  val pop : 'a stack -> 'a -> 'a stack
	      end

    val delay : ('a -> 'b) -> unit -> 'a -> 'b
    
end
