signature CONTRACTS =
sig

type ('a,'b) function = {requires : 'a -> bool,
                         ensures : 'b -> bool,
                         f : 'a -> 'b}

exception PreconditionFailure
exception PostconditionFailure
exception AssertionFailure

val assert : ('a, bool) function -> 'a -> unit

val call : ('a, 'b) function -> 'a -> 'b

val isTotal : ('a, 'b) function -> bool

val isTotalFn : ('a -> 'b) -> bool

val tautology : 'a -> bool

val fromFn : ('a -> 'b) -> ('a, 'b) function

val toFn : ('a, 'b) function -> 'a -> 'b


end
