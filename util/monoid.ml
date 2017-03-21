(** Monoid type and instances. *)

(** A monoid is given by a neutral element and a binary
    operation that should satisfy the laws:

{[
mappend x mempty = x
mappend mempty x = x
]}
*)
module T = struct
  type 't monoid =
    { mempty : 't
    ; mappend : 't -> 't -> 't
    }
end
include T
type 't t = 't monoid

(** Additive monoid. *)
let int_sum =
  { mempty = 0
  ; mappend = ( + )
  }

(** Multiplicative monoid. *)
let int_prod =
  { mempty = 1
  ; mappend = ( * )
  }

(** Additive monoid. *)
let float_sum =
  { mempty = 0.0
  ; mappend = ( +. )
  }

(** Multiplicative monoid. *)
let float_prod =
  { mempty = 1.0
  ; mappend = ( *. )
  }

(** Boolean monoid under conjunction. *)
let all =
  { mempty = true
  ; mappend = ( && )
  }

(** Boolean monoid under disjunction. *)
let any =
  { mempty = false
  ; mappend = ( || )
  }

(** Option monoid returning the leftmost non-Nothing value.  *)
let first =
  { mempty = None
  ; mappend = fun x y -> match x , y with
      | None , x -> x
      | Some _ as x , _ -> x
  }

(** Option monoid returning the rightmost non-Nothing value.  *)
let last =
  { mempty = None
  ; mappend = fun x y -> match x , y with
      | x , None -> x
      | _ , (Some _ as x) -> x
  }

(** List monoid *)
let list =
  { mempty = []
  ; mappend = (fun x y -> x @ y)
  }
