structure Tree : TREE =
struct

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
datatype traversal = InOrder | PreOrder | PostOrder
datatype branch = TLeft | TRight

exception Sub

structure VS = VectorSlice

fun empty () = Empty

fun isEmpty Empty = true
  | isEmpty _ = false

fun mapInOrder f Empty = Empty
  | mapInOrder f (Node (l,x,r)) = Node(mapInOrder f l, f x, mapInOrder f r)

fun mapPreOrder f Empty = Empty
  | mapPreOrder f (Node (l,x,r)) =
    let val x' = f x
    in Node(mapPreOrder f l, x', mapPreOrder f r) end

fun mapPostOrder f Empty = Empty
  | mapPostOrder f (Node (l,x,r)) =
    let val l' = mapPostOrder f l
        val r' = mapPostOrder f r
    in Node(l', f x, r') end

fun map InOrder = mapInOrder
  | map PreOrder = mapPreOrder
  | map PostOrder = mapPostOrder

fun sub (Empty, _) = raise Sub
  | sub (Node(_,x,_), []) = x
  | sub (Node(l,_,_), TLeft::dl) = sub (l, dl)
  | sub (Node(_,_,r), TRight::dl) = sub (r, dl)

fun flip Empty = Empty
  | flip (Node(l,x,r)) = Node(flip r, x, flip l)

fun reduce _ f i Empty = i
  | reduce ord f i (Node(l,x,r)) =
    case ord of
        InOrder => f (f (reduce ord f i l, x), reduce ord f i r)
      | PreOrder => f (f (x, reduce ord f i l), reduce ord f i r)
      | PostOrder => f (f (reduce ord f i l, reduce ord f i r), x)


fun max (a,b) = if a>b then a else b

fun depth Empty = 0
  | depth (Node(l,x,r)) = 1+max(depth l, depth r)


fun fromVectorSlice v =
    case VS.length v of
        0 => Empty
      | n => Node (fromVectorSlice (VS.subslice (v, 0, SOME (n div 2))),
                   VS.sub (v, n div 2),
                   fromVectorSlice (VS.subslice (v, (n div 2)+1, NONE)))


fun fromVector v = fromVectorSlice (VS.full v)

fun fromList l = fromVector (Vector.fromList l)

fun testFn () =
    let val counter = ref 0
    in fn x => (print ("counter: "^Int.toString (!counter)^
                       ", node: "^Int.toString x^"\n");
                counter:=(!counter)+1;
                x)
    end

fun toList' _ Empty acc = acc
  | toList' InOrder (Node(l,x,r)) acc =
    toList' InOrder l (x::toList' InOrder r acc)
  | toList' PreOrder (Node(l,x,r)) acc =
    x::(toList' PreOrder l (toList' PreOrder r acc))
  | toList' PostOrder (Node(l,x,r)) acc =
    toList' PostOrder l (toList' PostOrder r (x::acc))

fun toList ord t = toList' ord t []

fun toString strf Empty = "-"
  | toString strf (Node(l,x,r)) = "("^toString strf l^"<-"^
                                  strf x^"->"^toString strf r^")"


fun isLeaf (Node(Empty,_,Empty)) = true
  | isLeaf _ = false


fun exists p Empty = false
  | exists p (Node (l,x,r)) = p x orelse exists p l orelse exists p r

fun all p Empty = true
  | all p (Node (l,x,r)) = p x andalso all p l andalso all p r


fun root t = sub (t, [])
end
