structure LazySeq : SEQUENCE =
struct

open AthenaCore
open AthenaAsync
open AthenaAsync.Task
infix <| <|| await
infixr |> ||>

type 'a seq = 'a task Seq.seq

type 'a pair = 'a Seq.pair

fun length x = Seq.length x
fun null x = Seq.null x
fun nth n x = Seq.nth n x await (fn v => v)
fun tabulate f n = 


end
