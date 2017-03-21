(** Generic string conversion.

   The function {!show} is extensible, the default case is generic,
   but one may override the generic behaviour for specific types.
*)

open Generic_core
open Ty.T

(** Generic function *)
val show_default : 'a ty -> 'a -> string

(** An extensible function whose default behaviour is given by {!show_default} *)
val show : 'a ty -> 'a -> string

(** Add a ad-hoc case for {!show}. *)
val show_ext :'a ty -> string Consumer.ty_fun -> unit
