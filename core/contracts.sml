structure Contracts : CONTRACTS  =
struct

  type ('a, 'b) function = {requires : 'a -> bool,
                            ensures : 'b -> bool,
                            f : 'a -> 'b}

  exception PreconditionFailure
  exception PostconditionFailure
  exception AssertionFailure


  fun tautology _ = true

  fun callWithoutContracts (f:('a,'b) function) x = (#f f) x

  fun callWithContracts ({requires = precond,
                         ensures = postcond,
                         f = f}:('a,'b) function) x =
      case precond x of
          false => raise PreconditionFailure
        | true => let val y = f x
                  in case postcond y of
                         false => raise PostconditionFailure
                       | true => y
                  end



  fun call f x = case Env.debug of
                     Env.CONTRACT => callWithContracts f x
                   | _ => callWithoutContracts f x


  fun assert f x = case Env.debug of
                       Env.CONTRACT => (case call f x of
                                            false => raise AssertionFailure
                                          | true => ())
                     | _ => ()

  fun fromFn f = {requires = tautology, ensures = tautology, f = f}

  fun toFn f = call f


  fun isTotal f = true
  fun isTotalFn f = true

end
