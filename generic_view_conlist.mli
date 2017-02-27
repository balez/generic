(**
The list of constructor view underlies the RepLib Haskell library.
The view sees all types as variants. Products and records are viewed as variants of a single constructor. The other categories of types do not fit well under that description, and are left as base cases with no constructors.
*)

open Generic_core
open Ty.T

type 'a view = 'a Desc.Con.t list
type 'a t = 'a view

val view : 'a ty -> 'a view

(** @raise Not_found if the constructor is not in the list
{[
   conap cs x = Conap (c,y)  ==>  c.embed y = x
]}
*)
val conap : 'a Desc.Con.t list -> 'a -> 'a Desc.Con.conap
