(** Operations on option types. *)

open Generic_util

(** [option f] is the functorial action of the functor ['a option]:
{[Some x -> Some (f x)]}
{[None -> None]}
 *)
let option f = function
  | Some x -> Some (f x)
  | None -> None

(** [unopt s n] eliminates an option value by replacing the constructor [Some] by [s] and [None] by [n].
 *)
let unopt n s = function
  | None -> n
  | Some x -> s x

(** Partial function {[get_some (Some x) = x]}
@raise Failed on None
*)
let get_some = function
  | Some x -> x
  | None -> raise Exn.Failed

(** [opt_try x] forces the lazy value [x]. If any exception is raised,
[None] is returned, otherwise [Some x] is returned.
*)
let opt_try x =
  let debug = false in (* during development it may be useful to display the exception *)
  if debug then Some (Lazy.force x)
  else
    try Some (Lazy.force x)
    with _ -> None

(** [some_if f x = Some x] if [f x] is true, [None] otherwise *)
let some_if f x =
  if f x then Some x else None

(** [unopt_try s n x = unopt s n (opt_try x)] *)
let unopt_try n s x =
  unopt n s (opt_try x)

(** Monadic plus, the left argument has priority on the right
    in case both options have a value. *)
let leftist_plus x y = match x, y with
  | Some _ , _ -> x
  | None , Some _ -> y
  | None , None -> None
