structure AthenaCore : ATHENA_CORE =
struct

open Core

structure Env = Env
structure Stack = Stack
structure TopLevel = Core
structure Tree = Tree
structure Serialiser = Serialiser
structure Utf8Char = Utf8Char
structure Utf8String = Utf8String
structure Contracts = Contracts
structure Heap = Heap
structure UUID = UUID

structure Word = AthenaWord
structure Word8 = AthenaWord8
structure Word32 = AthenaWord32
structure Word64 = AthenaWord64


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
 

functor RBTFn (T:ORD_KEY) = RedBlackTree(T)
functor BSTFn (T:ORD_KEY) = BST(T)
