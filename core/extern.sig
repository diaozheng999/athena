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

end
