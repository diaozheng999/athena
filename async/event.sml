structure Event : EVENT =
struct

open Task
open AthenaCore
open AthenaCore.Serialiser

infix 3 await
infix 3 <| <||
infixr 3 |> ||>

structure T = Task

exception EventNotFound
exception ListenerNotFound

exception Undefined

type listener = serialised -> unit task
type event = (int ref * (int, listener) HashTable.hash_table ref)
type registry = (string, event) HashTable.hash_table

val ctx = "Athena.Async.Event"

fun undefined _ = raise Undefined

fun mkRegistry () = HashTable.mkTable
                        (HashString.hashString, (op=)) 
			(313, EventNotFound)

fun mkEvent () : event = (ref 0, 
			  ref (HashTable.mkTable 
				   (Word.fromInt, (op=))
                                   (17, ListenerNotFound)))



val addListener  = ref (fn (_:string) => fn (_:listener) => yield ~1)
val removeListener = ref (fn (_:string) => fn (_:int) => yield ())
val raiseEvent = ref (fn (_:string, _:serialised) => yield ())



fun startEventSystem (reg:registry) =

    let fun addListener' (event:string) listener =
	    (debug (ctx^".addListener", "adding listener to event \""
					^ event ^"\"");
	      case HashTable.find reg event of
                  NONE => (HashTable.insert reg (event, mkEvent ());
                           addListener' event listener)
		| (SOME (n, ref ldic)) => yield (n:= !n + 1;
						 HashTable.insert ldic
                                                                  (!n, listener);
						 debug (ctx^".addListener",
							"listener "^
							Int.toString (!n)^" added to event \""
							^ event ^"\"");
						 !n))

        fun removeListener' event lid =
	    (debug (ctx^".removeListener",
		    "removing listener "^Int.toString lid^" from event \""
		    ^ event ^"\"");
            case HashTable.find reg event of
                NONE => yield ()
              | SOME (n, ref listeners) =>
                yield (General.ignore (HashTable.remove listeners lid);
		       debug (ctx^".removeListener",
			      "removed.")))
	    handle _ => (debug(ctx^".removeListener",
			       "listener "^Int.toString lid^" from event \""
			       ^ event ^"\" not found.");
			 raise Fail "Listener not found.")
		

        fun raiseEvent' (event, arg) =
            case HashTable.find reg event of
                NONE => yield ()
              | SOME (_, ref listeners) =>
                let val tasklist = Vector.fromList
                                       (HashTable.listItems listeners)
                in (debug(ctx^".raiseEvent",
			  "raising event \""^event^"\" to "
			  ^Int.toString (Vector.length tasklist)^
			  " listener(s) with data stream "
			  ^Serialiser.toString "" arg);
		    ignore (concurrent
				(Vector.map (fn t => t arg) tasklist) ())) end

        val () = (addListener := addListener';
                  removeListener := removeListener';
                  raiseEvent := raiseEvent')

    in yield () end

end
