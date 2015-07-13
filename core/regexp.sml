structure RegExp    =
struct

open Tree

datatype 'a regexp = Zero
                   | All
                   | Char of 'a
                   | Or of 'a regexp * 'a regexp
                   | And of 'a regexp * 'a regexp
                   | Star of 'a regexp

                   (* the following definitions could be further simplified
                      but will is present for ease of computation *)
                   | Range of 'a * 'a
                   | Member of 'a tree
                   | Predicate of 'a -> bool

datatype 'a tok = TDot
                | TBracketOpen
                | TBracketClose
                | TBracketNegOpen
                | TStart
                | TEnd
                | TSubexprOpen
                | TSubexprClose
                | TStar
                | TRepOpen
                | TRepClose
                | TQnMark
                | TPlus
                | TPipe
                | TDigit
                | TWord
                | TNonWord
                | TWordBoundary
                | TNonDigit
                | TWhitespace
                | TNonWhitespace
                | TSpan
                | TChar of 'a


datatype ctx = CtxPattern | CtxBracketExpr


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

open Utf8Helper
(*
fun lex ctx getc strm =
    let fun interpret {pat=ptok, bra=btok} strm' =
            Option.map
                (fn tl => case ctx of
                              CtxPattern => ptok::tl
                            | _ => btok::tl)
                (lex ctx getc strm')
    in
      case getc strm of
          NONE => SOME []
        | SOME (#"^", rest) => interpret {pat=TStart, bra=(TChar #"^")} rest
        | SOME (#"$", rest) => interpret {pat=TEnd,   bra=(TChar #"$")} rest
        | SOME (#".", rest) => interpret {pat=TDot,   bra=(TChar #".")} rest
        | SOME (#"*", rest) => interpret {pat=TStar,  bra=(TChar #"*")} rest
        | SOME (#"?", rest) => interpret {pat=TQnMark,bra=(TChar #"?")} rest
        | SOME (#"+", rest) => interpret {pat=TPlus,  bra=(TChar #"+")} rest
        | SOME (#"]", rest) =>
          (case ctx of
               CtxBracketExpr => Option.map (fn tl => T
        | SOME (c, rest) => Option.map (fn tl => TChar c::tl)
                                       (lex ctx getc rest)
    end
*)
end
