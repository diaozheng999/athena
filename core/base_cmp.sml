structure Cmp =
struct

structure Int : ORD_KEY =
struct
type ord_key = int
val compare = Int.compare
end

structure Char : ORD_KEY =
struct
type ord_key = char
val compare = Char.compare
end

structure String : ORD_KEY =
struct
type ord_key = string
val compare = String.compare
end

structure Utf8Char : ORD_KEY =
struct
type ord_key = Utf8Char.char
val compare = Utf8Char.compare
end

structure Utf8String : ORD_KEY =
struct
type ord_key = Utf8String.string
val compare = Utf8String.compare
end

end
