signature ATHENA_CORE_TOPLEVEL = CORE

signature ATHENA_CORE_SERIALISABLE = SERIALISABLE
signature ATHENA_CORE_POLY_SERIALISABLE = POLY_SERIALISABLE
signature ATHENA_CORE_ASYNC_SERIALISABLE = ASYNC_SERIALISABLE
signature ATHENA_CORE_ASYNC_POLY_SERIALISABLE
= ASYNC_POLY_SERIALISABLE

signature ATHENA_CORE =
sig

    include CORE

    structure Env : ENV
    structure Stack : STACK
    structure TopLevel : CORE
    structure Serialiser : SERIALISER
    structure Char : ATHENA_CHAR
    structure String : ATHENA_STRING
    structure Tree : TREE
    structure Heap : HEAP
    structure Utf8Char : ATHENA_CHAR
    structure Utf8String : ATHENA_STRING
    structure Bit : ATHENA_WORD
    structure Word : ATHENA_WORD
    structure Word8 : ATHENA_WORD
    structure Word32 : ATHENA_WORD
    structure Word64 : ATHENA_WORD
    structure WordCvt : WORD_CVT
    structure Contracts : CONTRACTS
    structure UUID : UUID
    structure Exn : ATHENA_EXN
    structure Cmp : sig
                structure Int : ENUM_KEY
                structure Char : ENUM_KEY
                structure String : ORD_KEY
                structure Utf8Char : ENUM_KEY
                structure Utf8String : ORD_KEY
              end
end
