signature SERIALISER =
sig
    type serialised = Word8ArraySlice.slice
    datatype type_repr = GENERIC | T of Word8.word
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
             strictly type-checked. Thus, they should not be used if the serialised
             values are not to be immediately unserialised.  *)
    val packList : ('a -> serialised) -> 'a list -> serialised
    val packVector : ('a -> serialised) -> 'a vector -> serialised
    val packArray : ('a -> serialised) -> 'a array -> serialised
    val packOption : ('a -> serialised) -> 'a option -> serialised

    (* The following primitive types are unpacked according to these functions.
       Exception Type will be raised if the serialised value isn't of the correct type. *)
    val unpackInt : serialised -> int
    val unpackLargeInt : serialised -> IntInf.int
    val unpackBool : serialised -> bool
    val unpackUnit : serialised -> unit
    val unpackChar : serialised -> char
    val unpackString : serialised -> string
    val unpackReal : serialised -> real
    val unpackOrder : serialised -> order

    (* The following polymorphic types are unpacked according to these functions.
       Exception Type will be raised if the serialised value isn't of the correct type.
       NOTE: under current implementation, unpacking vectors and arrays are not guaranteed
             to fail given a wrongly serialised object. *)
    (* NOTE: unpackVectorElement and unpackArrayElement does not perform bounds check *)
    val unpackList : (serialised -> 'a) * (serialised -> int) -> serialised -> 'a list
    val unpackVector : (serialised -> 'a) -> serialised -> 'a vector
    val unpackVectorLength : serialised -> int
    val unpackVectorElement : (serialised -> 'a) -> serialised -> int -> 'a
    val unpackArray : (serialised -> 'a) -> serialised -> 'a array
    val unpackArrayElement : (serialised -> 'a) -> serialised -> int -> 'a
    val unpackArrayLength : serialised -> int
    val unpackOption : (serialised -> 'a) -> serialised -> 'a option


    (* The following functions return the sizes of primitive types. This is especially
       useful in checking of the size of variable-length  serialised objects such as
       strings or largeInts. *)
    val sizeInt : serialised -> int
    val sizeLargeInt : serialised -> int
    val sizeBool : serialised -> int
    val sizeUnit : serialised -> int
    val sizeChar : serialised -> int
    val sizeString : serialised -> int
    val sizeReal : serialised -> int
    val sizeOrder : serialised -> int


    (* The following functions return the size of polymorphic datatypes. *)
    (* NOTE : sizeVectorElement and sizeArrayElement does not perform bounds check. *)
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

end
