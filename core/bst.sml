functor BST (T:ORD_KEY) : BST =
struct


open Tree
structure VS = VectorSlice
structure T = T
type t = T.ord_key
type bst = t tree

exception Exist

fun insert (Empty, elem) = Node (Empty, elem, Empty)
  | insert (Node(l,x,r), elem) =
    case T.compare (elem, x) of
        EQUAL => raise Exist
      | LESS => Node(insert (l, elem),x,r)
      | GREATER => Node(l,x,insert (r, elem))

fun getSmallest Empty = NONE
  | getSmallest (Node(Empty,x,_)) = SOME x
  | getSmallest (Node(l,_,_)) = getSmallest l

fun getLargest Empty = NONE
  | getLargest (Node(_,x,Empty)) = SOME x
  | getLargest (Node(_,_,r)) = getLargest r

fun reduce f i t = Tree.reduce PostOrder f i t

fun find (Empty, _) = NONE
  | find (Node(l,x,r),elem) =
    case T.compare (elem, x) of
        EQUAL => SOME x
      | LESS => find (l,elem)
      | GREATER => find (r,elem)

fun fromList [] = Empty
  | fromList (x::xs) = insert (fromList xs, x)

fun fromVectorSlice vs =
    case VS.length vs of
        0 => Empty
      | _ => insert (fromVectorSlice (VS.subslice (vs,1,NONE)),
                     VS.sub(vs, 0))

fun fromVector v = fromVectorSlice (VS.full v)

fun toList t = Tree.toList InOrder t

fun remove (Empty, _) = raise Exist
  | remove (Node(l,x,r), elem) =
    case T.compare (elem,x) of
        LESS => Node(remove (l,elem), x, r)
      | GREATER => Node(l,x,remove (r,elem))
      | EQUAL => case (getLargest l, getSmallest r) of
                     (NONE, NONE) => Empty
                   | (SOME y, _ ) => Node(remove (l,y),y,r)
                   | (_ , SOME y) => Node(l,y,remove (r,y))

fun isValid Empty = true
  | isValid (Node(l,x,r)) = all (fn e => T.compare (e,x) = LESS) l andalso
                            all (fn e => T.compare (e,x) = GREATER) r andalso
                            isValid l andalso isValid r
    (* order invariant *)


end
