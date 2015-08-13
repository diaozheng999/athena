structure UUID :> UUID  =
struct

structure W32 = Word32
structure W8 = Word8
structure V = Word8Vector
structure VS = Word8VectorSlice

type uuid = V.vector
datatype generator = TIME | RAND | RAND_T
datatype expr = BINARY | HEX | UUID

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

fun affixVersion uuid =
    let fun mask (a,b) = Word8.orb(Word8.andb(a,0wxF),b)
	fun choice l =
	    let val len = List.length l
		val v = Word8.toInt (V.sub (random 1, 0))
	    in Vector.sub (Vector.fromList l, v mod len) end
	val v = [0wx80,0wx90,0wxa0,0wxb0]
    in
	V.tabulate (16, (fn 6 => mask (V.sub(uuid,6), 0wx40)
			| 8 => mask (V.sub(uuid, 8), choice v)
			| i => V.sub(uuid, i)))
    end

fun generate () = affixVersion (random 16)


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
  | toString HEX s = Serialiser.toString "" (Word8VectorSlice.full s)
  | toString GUID s = String.map
                          (Char.toLower)
                          (List.foldr (fn (a,b) => a^"-"^b) ""
                                      (List.map (Serialiser.toString "")
                                                [VS.slice (s,0,SOME 4),
                                                 VS.slice (s,4,SOME 2),
                                                 VS.slice (s,6,SOME 2),
						 VS.slice (s,8,SOME 2)])
                           ^ Serialiser.toString "" (VS.slice(s,10,NONE)))


fun fromString s =
    let val s = String.concat (String.tokens (fn #"-"=>true | _ =>false) s)
    in
      V.tabulate
          (size s div 2,
           (fn i => Option.valOf (W8.fromString (String.extract(s,i*2,SOME 2)))
          ))
    end handle _ => raise Option

end
