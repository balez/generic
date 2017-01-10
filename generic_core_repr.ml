open Generic_core
open Generic_util
open Ty.T
open App.T

(** [(a,b) repr_by] explains how [a] is {i represented} by [b]. *)
type ('a,'b) repr_by =
  { repr_ty : 'b ty (** Representation type. *)
  ; to_repr : 'a -> 'b (** Conversion from the abstract type to the representation. *)
  ; from_repr : 'b -> 'a option (** Partial conversion from representation to the abstract type. May fail if the representation isn't valid. *)
  ; default : 'a (** Default value, used temporarily when converting cyclic data. *)
  ; update : 'a -> 'b -> unit (** Restore the cycles. *)
  }
type 'a repr = Repr : ('a,'b) repr_by -> 'a repr
type 'a t = 'a repr
type tag = Tag
type (_,_) app += App : 'a repr -> ('a, tag) app

let unapp = function
  | App x -> x
  | _ -> invalid_arg "Generic_core_repr.unapp"

(** Type-indexed function that associates a representation to an abstract type. *)
type repr_fun =
  { f : 'a . 'a ty -> 'a repr }

let repr_closure = Extensible.create "repr" (* private *)
let repr t = unapp (repr_closure.f t)
let ext t f = repr_closure.ext t { f = fun t -> App (f.f t) }
