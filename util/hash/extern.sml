structure AthenaUtilHash : ATHENA_UTIL_HASH =
struct

structure SHA256 = Hash(SHA256)

end

functor AthenaUtilHashFn (H : ATHENA_UTIL_HASH_ALGO) =
Hash(H)
