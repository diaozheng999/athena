structure Exn : ATHENA_EXN =
ExnFn
(
  struct
  type exn = exn
  val history = MLton.Exn.history
  end
)
