structure UUID :> UUID  =
struct

structure W32 = Word32
structure V = Word8Vector
structure VS = Word8VectorSlice

type uuid = V.vector
datatype generator = TIME | RAND | RAND_T
datatype expr = BINARY | HEX | UUID | BASE64

val timer = Timer.totalRealTimer ()

fun getTime () = W32.fromLargeInt
                     (IntInf.mod(Time.toMilliseconds
                                    (Timer.checkRealTimer timer),
                                 4294967296))

val rand : Random.rand option ref = ref NONE

fun pseudo () =
    case !rand of
      NONE => (rand := SOME (Random.rand (W32.toInt (getTime ()), 453));
               pseudo ())
    | SOME r => Random.randInt r

fun random bytes =
    let val urand = BinIO.openIn "/dev/urandom"
        val rv = BinIO.inputN(urand, bytes)
        val _ = BinIO.closeIn urand
    in rv end
    handle Io => V.tabulate (bytes, (fn _ => Word8.fromInt (pseudo ())))


fun generate () = random 16


fun compare (a,b) = String.compare (Byte.bytesToString a,
                                    Byte.bytesToString b)

fun a < b = case compare (a,b) of
              LESS => true
            | _ => false

fun a > b = case compare (a,b) of
              GREATER => true
            | _ => false

fun a <= b = case compare (a,b) of
               GREATER => false
             | _ => true

fun a >= b = case compare (a,b) of
               LESS => false
             | _ => true


fun toString BINARY s = Serialiser.bitVectorToString
                            (Serialiser.toBitVector (Word8VectorSlice.full s))
  | toString BASE64 s = Base64.encode s
  | toString HEX s = Serialiser.toString "" (Word8VectorSlice.full s)
  | toString GUID s = String.map
                          (Char.toLower)
                          (List.foldr (fn (a,b) => a^"-"^b) ""
                                      (List.map (Serialiser.toString "")
                                                [VS.slice (s,0,SOME 4),
                                                 VS.slice (s,4,SOME 2),
                                                 VS.slice (s,6,SOME 2)])
                           ^ Serialiser.toString "" (VS.slice(s,8,NONE)))


fun fromString s = raise Fail "NYI"
end
