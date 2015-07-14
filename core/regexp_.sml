functor RegExp (T : ENUM_KEY) : REGEXP =
struct

structure T = T
structure Search = RedBlackTree (T)

type t = T.ord_key

datatype regexp = Zero
                | All
                | Char of t
                | Or of regexp * regexp
                | And of regexp * regexp
                | Star of regexp
                | Range of t * t
                | Elem of Search.bst
                | Fn of t -> bool
                | Not of regexp


exception Match
exception Unserialisable
exception Impure

fun isSerialisable (Fn _) = false
  | isSerialisable All = true
  | isSerialisable Zero = true
  | isSerialisable (Char _) = true
  | isSerialisable (NotChar _) = true
  | isSerialisable (Range _) = true
  | isSerialisable (Elem _) = true
  | isSerialisable (Not r) = isSerialisable r
  | isSerialisable (Or (r,s)) = isSerialisable r andalso isSerialisable s
  | isSerialisable (And(r,s)) = isSerialisable r andalso isSerialisable s
  | isSerialisable (Star r) = isSerialisable r

fun isPure (Fn _) = false
  | isPure (Range _) = false
  | isPure (Not _) = false
  | isPure (Elem _) = false
  | isPure Zero = true
  | isPure All = true
  | isPure (Char _) = true
  | isPure (NotChar _) = true
  | isPure (Or(a,b)) = isPure a andalso isPure b
  | isPure (And(a,b)) = isPure a andalso isPure b
  | isPure (Star a) = isPure a

fun negate All = Zero
  | negate Zero = All
  | negate (Char t) = NotChar t
  | negate (NotChar t) = Char t
  | negate (Or(a,b)) = And(negate a, negate b)
  | negate (And(a,b)) = Or(negate a, negate b)
  | negate (Star a) = Star (negate a)
  | negate _ = raise Impure


fun purify (Fn _) = raise Unserialisable
  | purify (Elem t) = Search.reduce Or Zero t
  | purify (Not r) = negate (purify r)
  | purify (Range(s,t)) = Search.reduce Or Zero
                          (Search.fromList
                               (T.enum(s,t)))
  | purify (Or(a,b)) = Or(purify a, purify b)
  | purify (Star a) = Star (purify a)
  | purify (And(a,b)) = And(purify a, purify b)
  | purify r = r




end
