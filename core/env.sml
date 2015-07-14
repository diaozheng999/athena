functor EnvWrapper (T : sig
                      val compiler : string
                    end) : ENV =
struct

datatype debug_t = DEBUG | RELEASE | CONTRACT
datatype compiler_t = SMLNJ | MLTON | POLY | UNKNOWN

val debug = CONTRACT
val compiler =
    case T.compiler of
        "smlnj" => SMLNJ
      | "mlton" => MLTON
      | "poly" => POLY
      | _ => UNKNOWN

end
