structure AthenaVector : ATHENA_VECTOR =
struct

type 'a vector = 'a vector

open AthenaAsync.Task

fun fromList l = async Vector.fromList l

fun tabulate (i,f) = async Vector.tabulate (i, sync f)

fun length v = async Vector.length v

fun sub x = async Vector.sub x

fun update x = async Vector.update x

fun concat x = async Vector.concat x

fun appi x = async (Vector.appi (sync x))

fun app x = async (Vector.app (sync x))

fun mapi x = async (Vector.mapi (sync x))

fun map x = async (Vector.map (sync x))

fun foldli x i = async (Vector.foldli (sync x) i)

fun foldri x i = async (Vector.foldri (sync x) i)

fun foldl x i = async (Vector.foldl (sync x) i)

fun foldr x i = async (Vector.foldr (sync x) i)

fun findi x = async (Vector.findi (sync x))

fun find x = async (Vector.find (sync x))

fun exists x = async (Vector.exists (sync x))

fun all x = async (Vector.all (sync x))

fun collate x = async (Vector.collate (sync x))


end
