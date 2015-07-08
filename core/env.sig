signature ENV = 
sig
    datatype debug_t = DEBUG | RELEASE
    datatype compiler_t = MLTON | SMLNJ | POLY | UNKNOWN
    val debug : debug_t
    val compiler : compiler_t
end
