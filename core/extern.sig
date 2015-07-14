signature ATHENA_CORE_TOPLEVEL = CORE

signature ATHENA_CORE_SERIALISABLE = SERIALISABLE

signature ATHENA_CORE =
sig

    include CORE


    structure Env : ENV
    structure Stack : STACK
    structure Task : TASK
    structure TopLevel : CORE
    structure Serialiser : SERIALISER
    structure Char : ATHENA_CHAR
    structure String : ATHENA_STRING
    structure Utf8Char : ATHENA_CHAR
    structure Utf8String : ATHENA_STRING
    structure Cmp : sig
                structure Int : ORD_KEY
                structure Char : ORD_KEY
                structure String : ORD_KEY
                structure Utf8Char : ORD_KEY
                structure Utf8String : ORD_KEY
              end

end
