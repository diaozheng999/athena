structure Contracts : CONTRACTS =
struct

  type ('a, 'b) function = {requires : 'a -> bool,
                            ensures : 'b -> bool,
                            f : 'a -> 'b}

  exception PreconditionFailure
  exception PostconditionFailure
  exception AssertionFailure

  val assert = case Env.debug of
                   Env.RELEASE => (fn _ _ => ())
                 | _ => (fn f a => case f a of
                                       false => raise AssertionFailure
                                     | true => ())


  fun tautology _ = true

  val call = case Env.debug of
                 Env.CONTRACT =>
                 (fn {requires:pre, ensures:post,f:f} x =>
                     case pre x of
                         false => raise PreconditionFailure
                       | true =>
                         let val y = f x
                         in case post y of
                                false => raise PostconditionFailure
                              | true => y
                         end)
               | _ => fn f x => (#f f) x

  fun fromFn f = {requires = tautology, ensures = tautology, f = f}

  val toFn = case Env.debug of
                 Env.CONTRACT => call
               | _ => #f


end
