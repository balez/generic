(** Anti-unification for type patterns.

The anti-unifier of two types is the most specific type that is more general than both types.
For instance, the anti-unifier of [int list * string], [char list * string option] is ['a list * 'b]
where ['a] and ['b] are fresh type variables.

The function {!anti_unify} works on type
codes. Correspondingly, the anti-unifier of the previous
example [Pair (List Int, String)] and [Pair (List Char,
Option String)] is [Pair (List Any, Any)].
*)

open Generic_core

type 'a ty = 'a Ty.ty

let rec resolve_synonym : type a . a ty -> a ty
  = fun t -> match Desc_fun.view t with
             | Desc.Synonym (b, eq)
               -> (match eq with
                   | Equal.Refl -> resolve_synonym b)
             | _ -> t

(* Informally,
{[
anti_unify (C xs) (C ys) = C (map2 anti_unify xs ys)
anti_unify _ _ = Any
]}

[anti_unify_recurse] and [anti_unify_prod] are private functions, handling
generic recursion over the type term.
 *)
let rec anti_unify : type a b . a ty -> b ty -> a ty
  = fun x y ->
  anti_unify_recurse (resolve_synonym x) (resolve_synonym y)

and anti_unify_recurse : type a b . a ty -> b ty -> a ty
  = fun x y ->
  if Ty.eq x y then x (* short circuit the recursive unification *)
  else if Ty.neq (Ty.conpat x) (Ty.conpat y) then Ty.Any
  else match Ty_desc.conap x with
         Desc.Con.Conap (c, xs)
         -> c.embed (anti_unify_prod (c.args, xs, Ty_desc.subterms y))
and anti_unify_prod : type a . a Product.t * a * Ty.dyn list -> a
  = let open Product.T in function
  | Nil , (), [] -> ()
  | Cons (Ty.Ty _, ts), (x,xs), (Ty.Dyn (Ty.Ty _, y) :: ys)
    -> (anti_unify x y, anti_unify_prod (ts, xs, ys))
  | _ -> assert false
