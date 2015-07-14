functor RedBlackTree (T:ORD_KEY)  =
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


fun wellRed Empty = true
  | wellRed (Node(Node(_,(Red,_),_),(Red,_),_)) = false
  | wellRed (Node(_,(Red,_),Node(_,(Red,_),_))) = false
  | wellRed (Node(l,x,r)) = wellRed l andalso wellRed r

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


fun restoreLeft (Node(Node(a,(Red,w),b),(Red,x),c),(Black,y),Node(d,(Red,z),e))=
    (Node(Node(a,(Red,w),b),(Black,x),c),(Red,y),Node(d,(Black,z),e))
  | restoreLeft (Node(Node(a,(Red,w),b),(Red,x),c),(Black,y),d)=
    (Node(a,(Red,w),b),(Black,x),Node(c,(Red,y),d))
  | restoreLeft (Node(a,(Red,w),Node(b,(Red,x),c)),(Black,y),Node(d,(Red,z),e))=
    (Node(a,(Black,w),Node(b,(Red,x),c)),(Red,y),Node(d,(Black,z),e))
  | restoreLeft (Node(a,(Red,w),Node(b,(Red,x),c)),(Black,y),d)=
    (Node(a,(Red,w),b),(Black,x),Node(c,(Red,y),d))
  | restoreLeft t = t

fun restoreRight (Node(a,(Red,w),b),(Black,x),Node(c,(Red,y),Node(d,(Red,z),e)))
    = (Node(a,(Black,w),b),(Red,x),Node(c,(Black,y),Node(d,(Red,z),e)))
  | restoreRight (b,(Black,x),Node(c,(Red,y),Node(d,(Red,z),e)))
    = (Node(b,(Red,x),c),(Black,y),Node(d,(Red,z),e))
  | restoreRight (Node(a,(Red,w),b),(Black,x),Node(Node(c,(Red,y),d),(Red,z),e))
    = (Node(a,(Black,w),b),(Red,x),Node(Node(c,(Red,y),d),(Black,z),e))
  | restoreRight (b,(Black,x),Node(Node(c,(Red,y),d),(Red,z),e))
    = (Node(b,(Red,x),c),(Black,y),Node(d,(Red,z),e))
  | restoreRight t = t

fun insert (t,i) =
    let
      fun ins t =
          case t of
              Empty => Node(Empty, (Red, i), Empty)
            | Node(l, (c, x), r) =>
              case (c,T.compare(i,x)) of
                  (_, EQUAL) => raise Exist
                | (Red, LESS) => Node(ins l, (c, x), r)
                | (Black, LESS) => Node (restoreLeft (ins l, (c,x), r))
                | (Red, GREATER) => Node(l, (c, x), ins r)
                | (Black, GREATER) => Node (restoreRight (l, (c,x), ins r))
    in case ins t of
           Node(Node(a,(Red,x),b),(Red,y),c) =>
           Node(Node(a,(Red,x),b),(Black,y),c)
         | Node(a,(Red,x),Node(b,(Red,y),c)) =>
           Node(a,(Black,x),Node(b,(Red,y),c))
         | t => t
    end



fun remove (t,i) =
    let
      fun rem t =
          case t of
              Empty => raise Exist
            | Node(l,(c,x),r) =>
              case (c,T.compare(i,x)) of
                  (_, EQUAL) => (case (BST.getLargest l, BST.getSmallest r) of
                                     (NONE,NONE) => Empty
                                   | (SOME y, _) => Node(rem l,y,r)
                                   | (_, SOME y) => Node(l,y,rem r))
                | _ => raise Exist
    in rem t end



fun fromList l = raise Exist
fun fromVector v = raise Exist
fun fromVectorSlice vs = raise Exist

end
