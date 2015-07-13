signature BST =
sig

  structure T : ORD_KEY
  type t = T.ord_key

  type bst
  exception Exist

  val empty : unit -> bst

  val isEmpty : bst -> bool

  val insert : bst * t -> bst

  val remove : bst * t -> bst

  val depth : bst -> int

  val find : bst * t -> t option

  val reduce : (t * t -> t) -> t -> bst -> t

  val fromList : t list -> bst

  val fromVector : t vector -> bst

  val fromVectorSlice : t VectorSlice.slice -> bst

  val toList : bst -> t list

  val toString : (t -> string) -> bst -> string

  val getLargest : bst -> t option

  val getSmallest : bst -> t option

  val isValid : bst -> bool

end
