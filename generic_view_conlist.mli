open Generic_core
open Ty.T

type 'a view = 'a Desc.Con.t list
type 'a t = 'a view

val view : 'a ty -> 'a view option

(* @raise Not_found if the constructor is not in the list *)
val conap : 'a Desc.Con.t list -> 'a -> 'a Desc.Con.conap
