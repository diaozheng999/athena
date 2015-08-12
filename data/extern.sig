signature ATHENA_DATA =
sig

  structure Seq : SEQUENCE
  structure Stream : STREAM
  structure Vector : ATHENA_VECTOR

end


signature ATHENA_SEQUENCE =
sig
    include SEQUENCE
end

signature ATHENA_STREAM =
sig
    include STREAM
end
