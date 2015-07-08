signature STACK = 
sig
    type 'a stack

    (* returns a stack that represents an empty stack *)
    val empty : unit -> 'a stack

    (* checks if a stack is empty. Stack.empty () should always return true. *)
    val isEmpty : 'a stack -> bool

    (* pushes an element onto the stack, and returns a new stack
       if type 'a stack is a ref, then the returned ref should be the same
       as the input ref *)
    val push : 'a stack -> 'a -> 'a stack

    (* pops an element off the stack. Returns an element and the rest of the stack 
       if type 'a stack is a ref, then the returned ref should be the same
       as the input ref *)
    val pop : 'a stack -> ('a option * 'a stack)

    (* rev' a b pushes all the elements of a in reverse order into b. 
       if type 'a stack is a ref, then the returned ref should be the same as b *)
    val rev' : 'a stack -> 'a stack -> 'a stack

    (* reverses all the elements in the stack.
       if type 'a stack is a ref, then the returned ref should be the same
       as the input ref *)
    val rev : 'a stack -> 'a stack

    (* if type 'a stack is a ref, then clone returns a different ref that represents
       the same queue. with all the elements in the same order *)
    val clone : 'a stack -> 'a stack

    (* presents a list representation of the stack. With the first element on the
       outermost. *)
    val toList : 'a stack -> 'a list

    (* presents an array representation of the stack. With the first element at index 0 *)
    val toArray : 'a stack -> 'a array

    (* presents a vector representation of the stack. With the first element at sub 0 *)
    val toVector : 'a stack -> 'a vector
    
end
