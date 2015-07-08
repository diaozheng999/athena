signature QUEUE = sig

    type 'a queue
    
    (* returns a queue that represents an empty queue *)
    val empty : unit -> 'a queue

    (* checks if a queue is empty. Queue.empty () should always return true. *)
    val isEmpty : 'a queue -> bool

    (* pushes an element onto the queue, and returns the new queue.
       if type 'a queue is a ref, then the returned ref should be the same
       as the input ref *)
    val enq : 'a queue -> 'a -> 'a queue

    (* dequeues an element from the queue. Returns the element and the rest of the queue
       if type 'a queue is a ref, then the returned ref should be the same as the
       input ref *)
    val deq : 'a queue -> 'a option * 'a queue

    (* reverses all elements in the queue.
       oif type 'a queue is a ref, then the returned ref should be the same as the
       input ref *)
    val rev : 'a queue -> 'a queue

    (* if type 'a queue is a ref, then clone creates a new queue with all elements in the
       same position *)
    val clone : 'a queue -> 'a queue

    (* presents a list representation of the queue. With the first element in insertion order
       on the outermost *)
    val toList : 'a queue -> 'a list
    
    (* presents an array representation of the queue. With the first element in insertion order
       at index 0 *)
    val toArray : 'a queue -> 'a array
    
    (* presents a vector representation of the queue. With the first element in insertion order
       at sub 0 *)
    val toVector : 'a queue -> 'a vector

end
