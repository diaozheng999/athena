functor ExnFn (T : ATHENA_EXN_CTX
	      where type exn = exn) =
struct

open T
type exn = exn

val message = exnMessage
val name = exnName

fun dump e =
    ( print ("Exception "^name e^" occured.\n");
      print (message e^"\n");
      print ("History:\n");
      print (List.foldl (fn (a,"") => a
			| (a,b) => a^"\n"^b)
			""
			(history e)^"\n"))

end
