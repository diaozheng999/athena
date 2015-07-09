structure RegExp : REGEXP =
struct


datatype 'a regexp = Zero
                   | All
                   | Char of 'a
                   | Or of 'a regexp * 'a regexp
                   | And of 'a regexp * 'a regexp
                   | Star of 'a regexp

exception Match



fun mapPartial f r =
    case r of
      All => All
    | Zero => Zero
    | Char c => f c
    | Or (a,b) => Or (mapPartial f a, mapPartial f b)
    | And (a,b) => And (mapPartial f a, mapPartial f b)
    | Star a => Star (mapPartial f a)


fun map f r = mapPartial (Char o f) r

fun match eqf r getc strm k =
    case r of
      Char a => (case getc strm of
                   NONE => false
                 | SOME (c,cs) => eqf(a,c) andalso k cs)
    | All => (case getc strm of
                NONE => false
              | SOME (c,cs) => k cs)
    | Zero => false
    | And (a,b) => match eqf a getc strm
                         (fn rest => match eqf b getc rest k)
    | Or (a,b) => match eqf a getc strm k orelse
                  match eqf b getc strm k
    | Star a => k strm orelse
                match eqf a getc strm
                (fn rest => match eqf (Star a) getc rest k)

fun listReader [] = NONE
  | listReader (x::xs) = SOME (x, xs)

fun vectorSliceReader s =
    case VectorSlice.length s of
      0 => NONE
    | _ => SOME (VectorSlice.sub(s,0),
                 VectorSlice.subslice(s,1,NONE))

fun substrReader s =
    case CharVectorSlice.length s of
      0 => NONE
    | _ => SOME (CharVectorSlice.sub(s,0),
                 CharVectorSlice.subslice(s,1,NONE))

fun matchl r = match (op=) r listReader

fun matchv r v k = match (op=) r vectorSliceReader (VectorSlice.full v)
                   (k o VectorSlice.vector)

fun matchs r s k = match (op=) r substrReader (CharVectorSlice.full s)
                   (k o CharVectorSlice.vector)

fun matchus r (s:Utf8String.string) = matchv r s

end
