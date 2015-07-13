functor RedBlackTree (T:ORD_KEY) : BST =
struct

open Core
open Tree

structure T = T
datatype colour = Red | Black
structure U : ORD_KEY =
  struct
    type ord_key = colour * T.ord_key
    fun compare ((_,u),(_,v)) = T.compare (u,v)
  end

structure BST = BST(U)
type bst = BST.bst
type t = T.ord_key

exception Exist


val empty = BST.empty

val isEmpty = BST.isEmpty

fun find (T,elem) = Option.map cdr (BST.find (T,(Red, elem)))

fun reduce f i t = cdr (BST.reduce (fn ((_,x),(_,y))=>(Red, f(x,y))) (Red,i) t)

val getLargest = Option.compose (cdr, BST.getLargest)

val getSmallest = Option.compose (cdr, BST.getSmallest)

fun wellRed (Node(Node(_,(Red,_),_),(Red,_),_)) = false
  | wellRed (Node(_,(Red,_),Node(_,(Red,_),_))) = false
  | wellRed _ = true

fun blackHeight Empty = 0
  | blackHeight (Node(l,(Red,_),r)) =
    (case (l,r) of
         (Node _, _) => blackHeight l
       | (Empty, Node _) => blackHeight r
       | (Empty, Empty) => 0)
  | blackHeight (Node(l,(Black,_),r)) =
    (case (l,r) of
         (Node _, _) => 1+blackHeight l
       | (Empty, Node _) => 1+blackHeight r
       | (Empty, Empty) => 1)

fun isBlackHeight i Empty = i=0
  | isBlackHeight i (Node(l,(Red,_),r)) = isBlackHeight i l andalso
                                          isBlackHeight i r
  | isBlackHeight i (Node(l,(Black,_),r)) = isBlackHeight (i-1) l andalso
                                            isBlackHeight (i-1) r


fun isValid t = BST.isValid t andalso wellRed t andalso
                isBlackHeight (blackHeight t) t


fun toString f = BST.toString (fn (Red,t) => "(R)"^f t | (Black,t) => "(B)"^f t)

val toList = (List.map cdr) o BST.toList

fun insert (t,i) = raise Exist
fun remove (t,i) = raise Exist

fun fromList l = raise Exist
fun fromVector v = raise Exist
fun fromVectorSlice vs = raise Exist

end
