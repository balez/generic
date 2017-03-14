(** Functorial map. *)

open Generic_util
open App.T
open App

module T = struct
  type 'f functorial =
    { fmap : 'a 'b . ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app }
end
include T
type 'f t = 'f functorial

(** {2 Instances} *)


let id =
  { fmap = (fun f x -> Id (f (get_id x))) }

let const =
  { fmap = (fun _ x -> Const (get_const x)) }

let option =
  let map f = function
  | Some x -> Some (f x)
  | None -> None
  in
  { fmap = (fun f x -> Option (map f (get_option x))) }

let list =
  { fmap = (fun f x -> List (List.map f (get_list x))) }
