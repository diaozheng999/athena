signature REGEXP =
sig

  structure T : ORD_KEY
  structure Search : BST

  type t = T.ord_key

  datatype regexp = Zero
                  | All
                  | Char of t
                  | Or of regexp * regexp
                  | And of regexp * regexp
                  | Star of regexp
                  | NotChar of t

  (* non-pure datastructures for computation *)
                  | Range of t * t
                  | Elem of Search.bst
                  | Fn of t -> bool
                  | Not of regexp

  exception Match
  exception Unserialisable
  exception Impure

  val isSerialisable : regexp -> bool

  val isPure : regexp -> bool

  val purify : regexp -> regexp

  val match : regexp -> (t, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader



(*
  val match : ('a * 'a -> bool)
              -> 'a regexp
              -> ('a, 'b) StringCvt.reader
              -> 'b
              -> ('b -> bool)
              -> bool
  val matchl : ''a regexp -> ''a list -> (''a list -> bool) -> bool
  val matchv : ''a regexp -> ''a vector -> (''a vector -> bool) -> bool
  val matchs : char regexp -> string -> (string -> bool) -> bool
  val matchus : Utf8Char.char regexp ->
                Utf8String.string ->
                (Utf8String.string -> bool) -> bool
*)

(*
  val match : 'a regexp -> 'a list -> bool
  val matchv : 'a regexp -> 'a vector -> bool
  val matchs : char regexp -> string -> bool
  val matchus : Utf8Char.char regexp -> Utf8String.string -> bool
*)

(*
  val select :  'a list -> ('a regexp * (unit -> 'b)) list -> 'b
  val selectv: 'a vector -> ('a regexp * (unit -> 'b)) list -> 'b
  val selects: string -> (char regexp * (unit -> 'a)) list -> 'a
  val selectus : Utf8String.string ->
                 (Utf8Char.char regexp * (unit -> 'a)) list -> 'a
*)

  (*
  val compile : string -> char regexp
  val ucompile : Utf8String.string -> Utf8Char.char regexp
  val cucompile : string -> Utf8Char.char regexp
  val uccompile : Utf8String.string -> char regexp
*)
  val map : ('a -> 'b) -> 'a regexp -> 'b regexp
  val mapPartial : ('a -> 'b regexp) -> 'a regexp -> 'b regexp


end
