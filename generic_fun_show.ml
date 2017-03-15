(* Generic string conversion.

   We make the function extensible so that one may override the generic behaviour for specific types.
*)

open Generic
open Ty.T

(* extensible function whose default case is given by the generic function *)
let show_closure = Consumer.create "Generic_fun_show.show"
let show = show_closure.f
let show_ext = show_closure.ext

(* generic case, calls show recursively. *)
let show_default : type a . a ty -> a -> string
  = fun t x -> match t with
    | Int -> string_of_int x
    | Float -> string_of_float x
    | List a -> "[" ^ String.concat "; " (List.map (show a) x) ^ "]"
    | t -> match Desc_fun.view t with
      | _ -> "<value>"

let () = show_ext Any { f = show_default }
