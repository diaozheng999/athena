functor MkEnumKey (T:SEQ_KEY) : ENUM_KEY =
struct

open T
fun enum(f,g) =
    case compare(f,g) of
      EQUAL => []
    | LESS => f::(enum(succ f,g))
    | GREATER => enum(g,f)
end
