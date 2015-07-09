structure Utf8String :> ATHENA_STRING 
			    where type char = Utf8Char.char 
			    where type string = Utf8Char.char vector
=
struct
open Utf8Helper


type char = Utf8Char.char
type string = char vector

val maxSize =
    case Int.maxInt of
      SOME i => i
    | NONE => Word.toInt (0wx7FFFFFFF)

val empty : string = Vector.fromList []

val size = Vector.length
val sub = Vector.sub
fun extract s = (VectorSlice.vector o VectorSlice.slice) s
fun substring (s,i,j) = extract(s,i,SOME j)
val concat = Vector.concat
fun i ^ j = concat [i,j]

fun concatWith s [] = empty
  | concatWith s [x] = x
  | concatWith s (x::y::xs) = concatWith s (concat [x,s,y]::xs)


(* split : string -> (char * string) option *)
(* revsplit : string -> (char * string) option *)
fun split (s:string) = 
    case size s of
	0 => NONE
      | _ => SOME (sub(s,0), extract(s,1,NONE))

fun revsplit (s:string) =
    case size s of
	0 => NONE
      | n => SOME (sub(s,n-1), extract(s,0,SOME (n-1)))


val isEmpty = Option.isSome o split


fun str c = Vector.fromList [c]

val implode = Vector.fromList

fun explode s = Vector.foldr (op::) [] s

val map = Vector.map

fun translate f s = concat (List.map f (explode s))

fun fields f =
    Vector.foldr
	(fn (c, []) => [] 
	| (c, s::t) => 
	  case f c of
	      true => empty::(s::t)
	    | false => (str c ^ s)::t) [empty]
	
fun tokens f = List.filter isEmpty o fields f 

fun toString s = Vector.foldr String.^ "" (Vector.map Utf8Char.toString s)

fun scan getc strm = 
    case getc strm of
	NONE => SOME (empty, strm)
      | SOME _ =>
	case Utf8Char.scan getc strm of
	    NONE => NONE
	  | SOME (c, rest) =>
	    case scan getc rest of
		SOME (s, rest') => SOME (str c ^ s, rest')
	      | NONE => SOME (str c, rest)

(*
fun stringReader s = Option.map (fn (c,r) =>
				    (Utf8Char.chr (ord c),r)) 
				(Utf8Helper.stringReader s) *)

fun fromString s =
    case scan stringReader s of
	SOME (s', _) => SOME s'
      | NONE => NONE

val fromCString = Option.composePartial (fromString, String.fromCString)
val toCString = String.toCString o toString


(* NOTE: the following functions are for strict equality checks.
   do not use these functions for unicode equality *)

fun isPrefix s1 s2 =
    case (split s1, split s2) of
	(NONE, _) => true
      | (SOME _, NONE) => false
      | (SOME (c1, s1'), SOME (c2, s2')) =>
	case c1=c2 of
	    true => isPrefix s1' s2'
	  | false => false

fun isSubstring s1 s2 =
    case (split s1, split s2) of
	(NONE, _) => true
      | (SOME _, NONE) => false
      | (SOME (c1, s1'), SOME (c2, s2')) =>
	case c1=c2 of
	    true => isSubstring s1' s2'
	  | false => isSubstring s1 s2'

fun isSuffix s1 s2 = 
    case (revsplit s1, revsplit s2) of
	(NONE, _) => true
      | (SOME _, NONE) => false
      | (SOME (c1, s1'), SOME (c2, s2')) =>
	case c1=c2 of
	    true => isSuffix s1' s2'
	  | false => false

val collate = Vector.collate

fun compare (s1,s2) =
    case (split s1, split s2) of
	(NONE, NONE) => EQUAL
      | (NONE, SOME _) => LESS
      | (SOME _, NONE) => GREATER
      | (SOME (c1, s1'), SOME (c2, s2')) =>
	case Utf8Char.compare(c1,c2) of
	    EQUAL => compare (s1',s2')
	  | cmp => cmp


fun s1 < s2 =
    case compare (s1,s2) of
	LESS => true
      | _ => false

fun s1 > s2 =
    case compare (s1,s2) of
	GREATER => true
      | _ => false

fun s1 <= s2 =
    case compare (s1,s2) of
	GREATER => false
      | _ => true

fun s1 >= s2 =
    case compare (s1,s2) of
	LESS => false
      | _ => true
end
