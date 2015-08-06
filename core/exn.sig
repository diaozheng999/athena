signature ATHENA_EXN_CTX =
sig
    type exn
    val history : exn -> string list
end

signature ATHENA_EXN = 
sig

    include ATHENA_EXN_CTX
    val message : exn -> string
    val name : exn -> string
    val dump : exn -> unit

end
