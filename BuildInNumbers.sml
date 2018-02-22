
signature TLU_BuildInNumbers = TOP_LEVEL_UTIL
structure BuildInNumbers :> BUILDINNUMBERS =
struct

open Syntax

  fun bn X =
    case X of
        ("e", "nat") => true
      | ("s", "nat -> nat") => true
      | _ => false

  fun pObj ob =
    case Obj.prj ob of
        Atomic (H, S) =>
          (case H of
              (Const "s") => 1 + (pSpine S)
            | (Const "e") => 0
            | _ => 0
          )
      | _ => 0
  and pSpine sp =
    case Spine.prj sp of
        LApp (M, S) => pMonadObj M
      | _ => 0
  and pMonadObj mo =
    case MonadObj.prj mo of
        Bang N => pObj N
      | _ => 0

  val atomIsNumber = bn
  val formatNumber = Int.toString o pObj

end


