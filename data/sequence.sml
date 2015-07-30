structure Seq :> SEQUENCE =
struct

open AthenaCore
open AthenaAsync.Task
infix 3 await
infix 3 <|
infixr 3 |>

datatype 'a pair = Pair of 'a * 'a | Singleton of 'a

type 'a seq = 'a vector

val % = Vector.fromList

fun length x = yield (Vector.length x)

fun null x = length x await (fn len => yield (len = 0))

fun nth n x = yield (Vector.sub (x,n))

fun tabulate f n = (f |> concurrent (Vector.tabulate (n, f))) 0

fun map f x = (nth 0 |> f |> (concurrent (Vector.map (fn v => f v) x))) x

fun pairwise (x : 'a seq) : 'a pair seq task =
    let val n = Vector.length x
        val size = (n+1) div 2
    in
      tabulate (fn i => case (2*i+1 >= n) of
                          true => yield (Singleton (Vector.sub (x, 2*i)))
                        | false => yield (Pair (Vector.sub (x, 2*i),
                                                Vector.sub (x, 2*i+1)))) size
    end

fun combine p (Singleton x) = yield x
  | combine p (Pair x) = p x

fun combine2 p q (Singleton x) = q x
  | combine2 p q (Pair x) = p x

fun reduce' comb x =
    case Vector.length x of
      1 => nth 0 x
    | _ => (pairwise |> map comb |> reduce' comb) x

fun reduce1 f = reduce' (combine f)

fun reduce2 f g = reduce' (combine2 f g)

fun append (a,b) =
    case Vector.length a of
      0 => yield b
    | alen => tabulate (fn i => case i<alen of
                                  true => nth i a
                                | false => nth (i-alen) b)
                       (alen + Vector.length b)


fun reduce f i x =
    length x await
           (fn 0 => yield i
             | n => case n mod 2 of
                      0 => reduce1 f x
                    | _ => (reduce1 f <| append) (x,%[i]))



fun mapreduce mapping id reduction = map mapping |> reduce reduction id

fun toString stringify x =
    case Vector.length x of
      0 => yield "<nil>"
      | _ => (map stringify
                  |> (reduce2 (fn (x,y) => yield(x^", "^y)) (fn x => yield x))
                  |> (fn str => yield ("<"^str^">"))) x


fun repeat n v =  tabulate (fn i => yield v) n


fun empty () = yield (%[])

fun zip (a,b) =
    case (min (Vector.length a, Vector.length b)) of
      0 => empty ()
    | n => tabulate (fn i => yield (Vector.sub (a,i), Vector.sub (b,i))) n


fun flatten q = empty () await (fn empty => reduce append empty q)


fun filter p = map (fn x => p x await
                              (fn true => yield (%[x])
                                | false => yield (%[]))) |> flatten


fun split i seq =
    length seq await
           (fn len =>
               case (i, i=len) of
                 (0, _) => yield (%[], seq)
               | (_, true) => yield (seq, %[])
               | _ => join (tabulate (fn i => nth i seq) i,
                            tabulate (fn j => nth (j+i) seq) (len-i)))

fun take i seq = tabulate (fn i => nth i seq) i

fun drop i seq =
    case Vector.length seq-i of
      0 => empty ()
    |n => tabulate (fn j => nth (j+i) seq) n


fun singleton e = yield (%[e])

fun cons e eseq = singleton e await (fn e => append (e, eseq))

fun mapreduce1 mapper reduction = map mapper |> reduce1 reduction

fun exists pred = filter pred |> null

fun forall pred = filter pred |> null |> async not

fun isEmpty seq = yield (Vector.length seq = 0)

fun fromList l = yield (Vector.fromList l)

fun fromVector v = yield v

fun fromArray a = yield (Array.vector a)

end
