open Generic_core

open Ty.T

(*  "leq"
Partial lexical order on type patterns used to implement
extensible functions allowing nested patterns of any depths.

Any <= x

f (x0,..xn) <= f (y0,..yn)
  iff  x0 < y0 or (x0 == y0 and x1 < x2 ...)

 *)

let rec leq : type a b . a ty -> b ty -> bool
  = fun x ->
  function
  | Ty.Any -> true
  | y -> Ty.eq (Ty.conpat x) (Ty.conpat y)
      && lexico_leq (Ty_desc.subterms x , Ty_desc.subterms y)

and lexico_leq = function
  | Ty.Dyn (Ty.Ty _, x) :: txs
  , Ty.Dyn (Ty.Ty _, y) :: tys
    ->  leq x y && (Ty.neq x y || lexico_leq (txs, tys))
  | [], [] -> true
  | _      -> false

let rec no_free_var : type a . a ty -> bool
  = function
  | Ty.Any -> false
  | t -> List.for_all no_free_var_dyn (Ty_desc.subterms t)
and no_free_var_dyn = function
  | Ty.Dyn (Ty.Ty _, t) -> no_free_var t
  | _ -> false

let no_free_var' (Ty.E t) = no_free_var t
