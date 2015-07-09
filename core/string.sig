(* the following file merges the SMLNJ and MLTON CHAR and STRING signatures *)

signature ATHENA_STRING =
sig
    eqtype string
    eqtype char
    val maxSize : int
    val size : string -> int
    val sub : string * int -> char
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string
    val ^ : string * string -> string
    val concat : string list -> string
    val concatWith : string -> string list -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix    : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool
    val compare : string * string -> order
    val collate : (char * char -> order)
                  -> string * string -> order
    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool
				    
    val toString : string -> String.string
    val fromString : String.string -> string option
    val toCString : string -> String.string
    val fromCString : String.string -> string option

    val scan : (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
end


signature ATHENA_CHAR =
sig
    eqtype char
    eqtype string

    val minChar : char
    val maxChar : char
    val maxOrd : int
		     
    val ord : char -> int
    val chr : int -> char
    val succ : char -> char
    val pred : char -> char
			   
    val compare : char * char -> order
    val <  : char * char -> bool
    val <= : char * char -> bool
    val >  : char * char -> bool
    val >= : char * char -> bool
				
    val contains : string -> char -> bool
    val notContains : string -> char -> bool
					    
    val isAscii : char -> bool
    val toLower : char -> char
    val toUpper : char -> char
    val isAlpha : char -> bool
    val isAlphaNum : char -> bool
    val isCntrl : char -> bool
    val isDigit : char -> bool
    val isGraph : char -> bool
    val isHexDigit : char -> bool
    val isLower : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isUpper : char -> bool
			      
    val toString : char -> String.string
    val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
    val fromString : String.string -> char option
    val toCString : char -> String.string
    val fromCString : String.string -> char option
end
