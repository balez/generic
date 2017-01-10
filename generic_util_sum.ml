open Generic_core
open Ty.T

type empty (* empty type : no value *)
type ('a,'b) choice = L of 'a | R of 'b

type _ ty +=
 | Empty : empty ty
 | Choice : 'a ty * 'b ty -> ('a,'b) choice ty

let empty_elim (_ : empty) = assert false
(* it shouldn't be possible to execute that function since we cannot give it any actual argument *)
