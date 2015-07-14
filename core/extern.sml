structure AthenaCore : ATHENA_CORE =
struct

open Core

structure Env = Env
structure Stack = Stack
structure Task = Task
structure TopLevel = Core
structure Serialiser = Serialiser
structure Utf8Char = Utf8Char
structure Utf8String = Utf8String

structure Char :> ATHENA_CHAR
                      where type char = char
                      where type string = String.string
= Char

structure String :> ATHENA_STRING
                        where type string = string
                        where type char = Char.char
= String

structure Cmp = Cmp


end
