structure Utf8Helper =
struct


fun readn n getc strm =
    let fun readn' 0w0 strm acc = SOME (acc, strm)
          | readn' n strm acc =
            case getc strm of
              SOME (c, strm') => readn' (n-0w1) strm' (acc ^ (String.str c))
            | NONE => NONE
    in readn' n strm "" end



fun stringReader "" = NONE
  | stringReader s =
    SOME (String.sub (s,0), String.extract (s,1,NONE))


end
