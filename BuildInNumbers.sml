
signature TLU_BuildInNumbers = TOP_LEVEL_UTIL
structure BuildInNumbers :> BUILDINNUMBERS =
struct

(* TODO - find a way to make it generic - e.g. applicable for
 * - nat
 * - bin
 * - other types *)
(* TODO - clean - type checker has to verify that exactly those exist or they
 * are turned on autmatically and those keywords get blocked by the
 * typechecker
 * *)

open Syntax

  fun bn X =
    case X of
      (*   ("ee", "nat") => true *)
      (* | ("s", "nat -> nat") => true *)
        ("o", "bin -> bin") => true
      | ("i", "bin -> bin") => true
      | ("e", "bin") => true
      | _ => false

  fun pObj ob =
    case Obj.prj ob of
        Atomic (H, S) =>
          (case H of
              (* (Const "s") => 1 + (pSpine S) *)
              (Const "ee") => []
            | (Const "i") => ["1"] @ (pSpine S)
            | (Const "o") => ["0"] @ (pSpine S)
            | _ => []
          )
      | _ => []
  and pSpine sp =
    case Spine.prj sp of
        LApp (M, S) => pMonadObj M
      | _ => []
  and pMonadObj mo =
    case MonadObj.prj mo of
        Bang N => pObj N
      | _ => []

  fun toBin [] = 0
    | toBin ("1" :: xs) = 1 + 2 * (toBin xs)
    | toBin ("0" :: xs) = 2 * (toBin xs)
    | toBin _ = 1 (* Exception *)


  val atomIsNumber = bn
  val formatNumber = Int.fmt StringCvt.HEX o toBin o pObj

end


