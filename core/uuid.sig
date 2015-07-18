signature UUID =
sig

  eqtype uuid

  datatype generator = TIME | RAND | RAND_T
  datatype expr = BINARY | HEX | UUID | BASE64

  val generate : unit -> uuid

  val compare : uuid * uuid -> order

  val < : uuid * uuid -> bool

  val > : uuid * uuid -> bool

  val <= : uuid * uuid -> bool

  val >= : uuid * uuid -> bool

  val toString : expr -> uuid -> string

  val fromString : string -> uuid

end
