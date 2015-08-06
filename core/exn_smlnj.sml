structure Exn : ATHENA_EXN =
ExnFn
(
  struct
  type exn = exn
  val history = SMLofNJ.exnHistory
  end
)
