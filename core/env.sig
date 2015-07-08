signature ENV = 
sig
    datatype debug_t = DEBUG | RELEASE
    val debug : debug_t
end
