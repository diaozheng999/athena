(* uuid.sig
   @Diao Zheng

   signature UUID represents a universally unique identifier, a 128-bit
   value.
*)

signature UUID =
sig

  eqtype uuid

  datatype generator = TIME | RAND | RAND_T
  datatype expr = BINARY | HEX | UUID

  (* generate () will always return a unique uuid *)
  val generate : unit -> uuid

  (* compares two uuids in alphabetical order *)
  val compare : uuid * uuid -> order

  val < : uuid * uuid -> bool

  val > : uuid * uuid -> bool

  val <= : uuid * uuid -> bool

  val >= : uuid * uuid -> bool

  (* returns the string of an uuid, given the expression *)
  val toString : expr -> uuid -> string

  val random : int -> Word8Vector.vector

  (* returns a UUID from a string representation, which is either
     - lower-case hex notation:
          f47ac10b58cc4372a5670e02b2c3d479X...
     - upper-case hex notation:
          F47AC10B58CC4372A5670E02B2C3D479X...
     - lower-case canonical form:
          f47ac10b-58cc-4372-a567-0e02b2c3d479X...
     - upper-case canonical form:
          F47AC10B-58CC-4372-A567-0E02B2C3D479X... *)
  val fromString : string -> uuid

end
