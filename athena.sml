structure Athena =
struct

open AthenaCore

structure Core = AthenaCore
structure Async = AthenaAsync
structure Data = AthenaData
structure Util = AthenaUtil
structure Program = AthenaProgram

end

functor AthenaRBTFn (T:ORD_KEY) = RBTFn (T)
functor AthenaBSTFn (T:ORD_KEY) = BSTFn (T)
