functor EnvWrapper (T : sig
			val compiler : string
		    end) : ENV =
struct

datatype debug_t = DEBUG | RELEASE
datatype compiler_t = SMLNJ | MLTON | POLY | UNKNOWN

val debug = DEBUG
val compiler = 
    case T.compiler of
	"smlnj" => SMLNJ
      | "mlton" => MLTON
      | "poly" => POLY
      | _ => UNKNOWN

end
