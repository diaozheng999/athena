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
  FE - Extension
  FF - Unsafe
*)

(* How are the various types serialised:

  All serialisations has a header byte that indicates the type of the object
  in the serialisation.

  Unit - There'a only one entity. So it is just encoded with the header

  Bool - The body byte is represented by 00 (false) or 01 (true)

  Char - The body byte is represented by the ord of char

  Int - The body bytes are encoded in little endian order, for compatibility
        with x86 architectures

  Single - The body bytes are encoded with IEEE 754 floating point (Single)

  String - [ Hdr |-- len --| string ]
           First 4 bytes encode the length of the string in little endian
           The 5th byte onwards encode the string itself.

  LargeInt - [ Hdr |-- len/sgn --| int ]
             First 4 bytes encode the number of bytes the large int take.
                 - if the length part is negative, it means that the int
                   itself is a negative number
             The integer itself is encoded in big endian format so that
             error introduced for a partial transmission is reduced as the
             more significant bits are transmitted.

  List - nil : [ Hdr | 00 ]
         cons : [ Hdr | 01 | elem | [rest]]
         To facilitate recursion, two bytes are always used to identify the list
         be it nil or an element. If it is followed by an element, then the rest
         of the list. E.g. [[()],[]] is encoded as
              07 01 [07 01 00     07 00] 07 01        [07 00] 07 00
              \outer \inner \unit \nil   \outer cons   \nil   \outer nil

  Vector - [ Hdr | size | num_elem | ptr | val ]
           \ vec header/ \ vec body ==========/
           The vector is encoded by the vector header which includes the header
           byte (0x08) as well as the size (4 bytes) of the body sequence in
           little endian encoding. This is followed by the vector body.

           The vector body is encoded by:
            - First 4 bytes are the number of elements in the vector in little
              endian order.
            - The next (num_elem * 4) bytes are the pointers. Each encodes the
              offset of the nth element from the start of the vector body in a
              4-byte little endian integer (int32)

           E.g. packVector packInt #[1,2,3] is returned as :
                0  1        5        9        13       17       21
                08 1F000000 03000000 10000000 15000000 1A000000 0301000000
                \hdr \size  \num_elem \offs_0 \offs_1  \offs_2  \enc of 1
                26         31
                0302000000 0303000000
                \enc of 2  \enc of 3

  Array - encoded the same way as vector

  Order - the body is coded by 00 (EQUAL), 01 (GREATER) or FF (two's compiment
          -1, LESS)

  Option - NONE [0B 00], SOME v [0B 01 [v]]; similar concept to list

  Word - 4 bytes in little endian

  UTF-8 Char - encodes the code point in 3 bytes since valid valid codepoints
               are from U+0000 to U+10FFFF

  UTF-8 String - similar encoding to string, just encodes the encoded utf string

  Tree - Empty [Hdr 00], Node(l,x,r) [ Hdr | offs | [x] | [l] | [r] ]
         - behaves similar to list and options (since tree is algebraic as well)
         - for Node encoding, offset is encoded as a 32-bit little-endian int,
           and used to indicate the end of left branch/start of right branch
         - this is possible because the minimum [[x] | [l]] is 3 bytes long
           (achieved by Node(Empty,(),_):unit tree) offs has a min value of 8
           and can never be 0. So if this byte is 0, then it must be Empty.

         - E.g.    1       encoded as   0  1        5          10   12 13
                    \                   0F 0C000000 0301000000 0F00 0F 18000000
                     2                  \hdr \offs   \enc 1    \Empty \hdr \offs
                    / \                 17         22 23       27         32
                   3   4                0302000000 0F 0C000000 0303000000 0F00
                                        \enc 2     \hdr \offs   \enc 3    \Empty
                                        34   36 37       41         46   48
                                        0F00 0F 0C000000 0304000000 0F00 0F00
                                        \Empty \hdr \offs \enc 4    \Empty\Empty


  Double - body is encoded in IEEE 754 64-bit floating point (double)

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



    val packUnit : unit -> serialised
    val packBool : bool -> serialised
    val packChar : char -> serialised
    val packInt : int -> serialised
    val packReal : real -> serialised
    val packString : string -> serialised
    val packLargeInt : IntInf.int -> serialised
    val packOrder : order -> serialised
    val packReal64 : real -> serialised
    val packWord : Word32.word -> serialised
    val packUChar : Utf8Char.char -> serialised
    val packUString : Utf8String.string -> serialised

    (* The following polymorphic types are packed according to these functions.
       NOTE: under current implementation, vector and array packing are not
             strictly type-checked. Thus, they should not be used if the
             serialised values are not to be immediately unserialised.  *)



    val packList : ('a -> serialised) -> 'a list -> serialised
    val packVector : ('a -> serialised) -> 'a vector -> serialised
    val packArray : ('a -> serialised) -> 'a array -> serialised
    val packOption : ('a -> serialised) -> 'a option -> serialised
    val packTree : ('a -> serialised) -> 'a Tree.tree -> serialised



    (* The following primitive types are unpacked according to these functions.
       Exception Type will be raised if the serialised value isn't of the
       correct type. *)

    (* When a serialised object is unpacked. the remaining of the serialisation
       is returned along with the object. To just get the object, use car to get
       the first element of the tuple. E.g. to unpack an integer from
       serialisation s, execute  car (unpackInt s) *)


    val unpackUnit : serialised -> unit * serialised
    val unpackBool : serialised -> bool * serialised
    val unpackChar : serialised -> char * serialised
    val unpackInt : serialised -> int * serialised
    val unpackReal : serialised -> real * serialised
    val unpackString : serialised -> string * serialised
    val unpackLargeInt : serialised -> IntInf.int * serialised
    val unpackOrder : serialised -> order * serialised
    val unpackWord : serialised -> Word32.word * serialised
    val unpackUChar : serialised -> Utf8Char.char * serialised
    val unpackUString : serialised -> Utf8String.string * serialised

    (* The following polymorphic types are unpacked according to these
       functions. Exception Type will be raised if the serialised value isn't of
       the correct type.
       NOTE: under current implementation, unpacking vectors and arrays are not
             guaranteed to fail given a wrongly serialised object. *)
    (* NOTE: unpackVectorElement and unpackArrayElement does not perform bounds
             check *)

    val unpackList : (serialised -> 'a * serialised)
                     -> serialised
                     -> 'a list * serialised
    val getListElem : (serialised -> 'a * serialised)
                      -> serialised
                      -> 'a option * serialised

    val unpackVector : (serialised -> 'a * serialised)
                       -> serialised
                       -> 'a vector * serialised
    val getVectorSize : serialised -> int
    val getVectorElem : (serialised -> 'a * serialised)
                        -> serialised * int
                        -> 'a

    val unpackArray : (serialised -> 'a * serialised)
                      -> serialised
                      -> 'a array * serialised
    val getArraySize : serialised -> int
    val getArrayElem : (serialised -> 'a * serialised)
                       -> serialised * int
                       -> 'a


    val unpackOption : (serialised -> 'a * serialised)
                       -> serialised
                       -> 'a option * serialised

    val getOptionIsSome : serialised -> bool


    val unpackTree : (serialised -> 'a * serialised)
                     -> serialised
                     -> 'a Tree.tree * serialised
    val getTreeNode : (serialised -> 'a * serialised)
                      -> serialised
                      -> {value : 'a option,
                          left : serialised, right : serialised}



    val toString : string -> serialised -> string
    val toBitVector : serialised -> bool vector
    val bitVectorToString : bool vector -> string


    val toGeneric : serialised -> serialised
    val fromGeneric : serialised * type_repr -> serialised

    val cast : serialised * type_repr * type_repr -> serialised
    (* val cast' : serialised * type_id * type_id -> serialised *)

    (*
    val repr : type_repr -> type_id
    val id : type_id -> type_repr
    *)


    val getType : serialised -> type_repr
    (* val getType' : serialised -> type_id *)

    (* interprets an unheadered serialised object with a proper header *)
    val interpret : serialised * type_repr -> serialised

    val ensure : serialised -> int * type_repr 
		 -> (unit -> 'a) -> 'a

    (* changes the internal representation to a byte sequence *)
    val toBytes : serialised -> Word8VectorSlice.slice

    (* changes a byte sequence to the internal representation *)
    val fromBytes : Word8VectorSlice.slice -> serialised

    (* creates an valid internal repr from a bytes sequence.
       i.e. appending 0xfe to the front *)
    val extern : Word8VectorSlice.slice -> serialised
    val unpackExtern : serialised -> Word8VectorSlice.slice
end
