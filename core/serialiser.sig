signature SERIALISER =
sig
    type serialised = Word8VectorSlice.slice
    type type_repr = Word8.word

(* Header definitions
  00 - Unit
  01 - Bool
  02 - Char
  03 - Int
  04 - Single (32-bit real)
  05 - String
  06 - LargeInt
  07 - List
  08 - Vector
  09 - Array
  0A - Order
  0B - Option
  0C - Word
  0D - UTF-8 Char
  0E - UTF-8 String
  0F - Tree
  10 - Double (64-bit real)
  11 - Long (64-bit int)
  12 - Word64 (64-bit word)
  FE - Extensiono
  FF - Unsafe
*)

    datatype type_id = GENERIC
                     | INT
                     | LARGE_INT
                     | BOOL
                     | UNIT
                     | CHAR
                     | STRING
                     | REAL
                     | UTF_CHAR
                     | UTF_STRING
                     | ORDER of type_id
                     | TREE of type_id
                     | VECTOR of type_id
                     | ARRAY of type_id
                     | SERIALISED
                     | EXT
    exception Type

    (* The following primitive types are packed according to these functions.
       NOTE: under current implementation, packing of real may result
             in a loss of precision *)

    val packInt : int -> serialised
    val packLargeInt : IntInf.int -> serialised
    val packBool : bool -> serialised
    val packUnit : unit -> serialised
    val packChar : char -> serialised
    val packString : string -> serialised
    val packReal : real -> serialised
    val packOrder : order -> serialised

    (* The following polymorphic types are packed according to these functions.
       NOTE: under current implementation, vector and array packing are not
             strictly type-checked. Thus, they should not be used if the
             serialised values are not to be immediately unserialised.  *)
    val packList : ('a -> serialised) -> 'a list -> serialised
    val packVector : ('a -> serialised) -> 'a vector -> serialised
    val packArray : ('a -> serialised) -> 'a array -> serialised
    val packOption : ('a -> serialised) -> 'a option -> serialised

    (* The following primitive types are unpacked according to these functions.
       Exception Type will be raised if the serialised value isn't of the
       correct type. *)

    (* When a serialised object is unpacked. the remaining of the serialisation
       is returned along with the object. To just get the object, use car to get
       the first element of the tuple. E.g. to unpack an integer from
       serialisation s, execute  car (unpackInt s) *)
    val unpackInt : serialised -> int * serialised
    val unpackLargeInt : serialised -> IntInf.int * serialised
    val unpackBool : serialised -> bool * serialised
    val unpackUnit : serialised -> unit * serialised
    val unpackChar : serialised -> char * serialised
    val unpackString : serialised -> string * serialised
    val unpackReal : serialised -> real * serialised
    val unpackOrder : serialised -> order * serialised

    (* The following polymorphic types are unpacked according to these
       functions. Exception Type will be raised if the serialised value isn't of
       the correct type.
       NOTE: under current implementation, unpacking vectors and arrays are not
             guaranteed to fail given a wrongly serialised object. *)
    (* NOTE: unpackVectorElement and unpackArrayElement does not perform bounds
             check *)
    val unpackList : (serialised -> 'a) -> serialised -> 'a list * serialised
    val unpackVector : (serialised -> 'a) -> serialised ->
                       'a vector * serialised
    val unpackVectorLength : serialised -> int
    val unpackVectorElement : (serialised -> 'a) -> serialised -> int
                              -> 'a * serialised
    val unpackArray : (serialised -> 'a) -> serialised -> 'a array
    val unpackArrayElement : (serialised -> 'a) -> serialised -> int
                             -> 'a * serialised
    val unpackArrayLength : serialised -> int
    val unpackOption : (serialised -> 'a) -> serialised
                       -> 'a option * serialised


    (* The following functions return the sizes of primitive types. This is
       especially useful in checking of the size of variable-length serialised
       objects such as strings or largeInts. *)
    val sizeInt : serialised -> int
    val sizeLargeInt : serialised -> int
    val sizeBool : serialised -> int
    val sizeUnit : serialised -> int
    val sizeChar : serialised -> int
    val sizeString : serialised -> int
    val sizeReal : serialised -> int
    val sizeOrder : serialised -> int


    (* The following functions return the size of polymorphic datatypes. *)
    (* NOTE : sizeVectorElement and sizeArrayElement does not perform bounds
              check. *)
    val sizeList : (serialised -> int) -> serialised -> int
    val sizeVector : (serialised -> int) -> serialised -> int
    val sizeVectorElement : (serialised -> int) -> serialised -> int -> int
    val sizeArray : (serialised -> int) -> serialised -> int
    val sizeArrayElement : (serialised -> int) -> serialised -> int -> int
    val sizeOption : (serialised -> int) -> serialised -> int

    val toString : serialised -> string
    val toBitVector : serialised -> bool vector
    val bitVectorToString : bool vector -> string


    val toGeneric : serialised -> serialised
    val fromGeneric : serialised * type_repr -> serialised

    val cast : serialised * type_repr * type_repr -> serialised
    val cast' : serialised * type_id * type_id -> serialised

    val repr : type_repr -> type_id
    val id : type_id -> type_repr

    val getType : serialised -> type_repr
    val getType' : serialised -> type_id

    (* interprets an unheadered serialised object with a proper header *)
    val interpret : serialised * type_repr -> serialised

end
