(* Generic string conversion.

   We make the function extensible so that one may override the generic behaviour for specific types.
*)

open Generic
open Ty.T

(* generic case *)
let show_default : type a . a ty -> a -> string
  = function
    | Int -> string_of_int
    | Float -> string_of_float
    | t -> match Desc.view t with
    | _ -> fun _ -> "<value>"

(* extensible function whose default case is given by the generic function *)
let show_closure = Consumer.create "Generic_fun_show.show"
let show = show_closure.f
let show_ext = show_closure.ext
let () = show_ext Any { f = show_default }
