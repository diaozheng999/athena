signature ATHENA_GRAMMAR =
sig
    structure T : sig type 'a task end

    type expr
    type oper

    val lex : string -> expr list T.task
    val parse : string -> oper option T.task
    val compile : oper -> string T.task
end



signature ATHENA_OUTPUT =
sig
    structure Grammar : ATHENA_GRAMMAR

    type output

    val decompose : output -> Grammar.oper Grammar.T.Task
end

signature COMPILER =
sig
    structure Output : ATHENA_OUTPUT

    val parse : string -> Output.output Output.Grammar.T.task
    val compile : Output.output -> string Output.Grammar.T.task

end


