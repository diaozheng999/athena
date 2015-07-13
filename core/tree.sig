signature TREE =
sig

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
datatype traversal = InOrder | PreOrder | PostOrder
datatype branch = TLeft | TRight

val empty : unit -> 'a tree

val isEmpty : 'a tree -> bool

val map : traversal -> ('a -> 'b) -> 'a tree -> 'b tree

val flip : 'a tree -> 'a tree

val sub : 'a tree * branch list -> 'a

val reduce : traversal -> ('a * 'a -> 'a) -> 'a -> 'a tree -> 'a

val depth : 'a tree -> int

val fromVectorSlice : 'a VectorSlice.slice -> 'a tree

val fromVector : 'a vector -> 'a tree

val fromList : 'a list -> 'a tree

val toList : traversal -> 'a tree -> 'a list

val toString : ('a -> string) -> 'a tree -> string

val isLeaf : 'a tree -> bool

val exists : ('a -> bool) -> 'a tree -> bool

val all : ('a -> bool) -> 'a tree -> bool

end
