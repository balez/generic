(** Representation of abstract datatypes. *)

open Generic_core
open Ty.T

(** [(a,b) repr_by] explains how [a] is {i represented} by [b]. *)
type ('a,'b) repr_by =
  { repr_ty : 'b ty (** Representation type. *)
  ; to_repr : 'a -> 'b (** Conversion from the abstract type to the representation. *)
  ; from_repr : 'b -> 'a option (** Partial conversion from representation to the abstract type. May fail if the representation isn't valid. *)
  ; default : 'a (** Default value, used temporarily when converting cyclic data. *)
  ; update : 'a -> 'b -> unit (** Restore the cycles. *)
  }

(** The concrete representation type is existentially
    quantified. Thus the type [a repr] means: [a] is
    {i representable}. *)
type 'a repr = Repr : ('a,'b) repr_by -> 'a repr

type 'a t = 'a repr
(** A synonym, so that [repr] might be referred to as [Repr.t] *)

(** Type-indexed function that associates a representation to an abstract type. *)
type repr_fun =
  { f : 'a . 'a ty -> 'a repr }

val repr : 'a ty -> 'a repr
(** @return the representation associated to an abstract type. *)

val ext : 'a Ty.pat -> repr_fun -> unit
(** [ext t f] extends function [repr] with a new case for type [t] defined by [f]. *)
