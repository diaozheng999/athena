(* base_cmp.sml
   @Diao Zheng
   
   details the Cmp structure which is used in functors that require
   comparisons. 

   signature ENUM_KEY represents a enumerable key. I.e. suppose structure
   T ascribes to ENUM_KEY, then given values a,b of type T.ord_key, there
   exists a finite number of elements of type T.ord_key such that for all
   elements e, T.compare (e, a) <> LESS andalso T.compare (e, b) = GREATER
*)


structure Cmp =
struct

structure Int : ENUM_KEY =
struct

structure T = MkEnumKey(
              struct
              type ord_key = int
              val compare = Int.compare
              fun succ n = n+1
              end)
open T

end

structure Char : ENUM_KEY =
struct

structure T = MkEnumKey(
              struct
              type ord_key = char
              val compare = Char.compare
              val succ = Char.succ
              end)
open T

end

structure String : ORD_KEY =
struct
type ord_key = string
val compare = String.compare
end

structure Utf8Char : ENUM_KEY =
struct
structure T = MkEnumKey(
              struct
              type ord_key = Utf8Char.char
              val compare = Utf8Char.compare
              val succ = Utf8Char.succ
              end)
open T

end

structure Utf8String : ORD_KEY =
struct
type ord_key = Utf8String.string
val compare = Utf8String.compare
end

end
