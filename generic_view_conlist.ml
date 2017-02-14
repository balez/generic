(** List of constructors view.
    Records and products are viewed as a list of one constructor.
    Abstract types are dealt with by viewing their public representation.
    Synonyms are followed,
    Base types and custom types are ignored: Int, Int32, Int64, Char, Float, String, Bytes, Array.

    TODO, use records instead of products for the arguments of
    constructors, this way we would keep the meta information of
    field names.
*)

open Generic_core
open Generic_util
open Desc
open Ty.T
let ( -< ) = Fun.(-<)

type 'a view = 'a Con.t list option

(** @raise Exn.Failed if the representation doesn't correspond to a valid abstract value *)
let repr r (Con.Con {name; args; embed; proj}) =
  Con.Con { name; args
          ; embed = Option.get_some -< r.Repr.from_repr -< embed
          ; proj = proj -< r.to_repr
          }

let rec view : type a . a ty -> a view
  = fun t ->
    match Desc_fun.view t with
    | Product (p, iso) ->
      Some [Con { name  = ""
                ; args  = p
                ; embed = iso.fwd
                ; proj  = (fun x -> Some (iso.bck x))
                }
           ]
    | Record r ->
      Some [Con { name  = ""
                ; args  = Record.product r
                ; embed = r.iso.fwd
                ; proj  = (fun x -> Some (r.iso.bck x))
                }
           ]
    | Variant v -> Some (Variant.con_list v.cons)
    | Extensible e -> Some (Ext.con_list e)
    | Synonym (t', eq) -> (match eq with
          Equal.Refl -> view t')
    | Abstract ->
      (match Repr.repr t with
       | Repr.Repr r -> Option.map (List.map (repr r)) (view r.repr_ty))
    | _ -> None
