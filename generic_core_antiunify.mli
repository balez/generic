(** Anti-unification for type patterns. *)

open Generic_core

val resolve_synonym : 'a Ty.ty -> 'a Ty.ty
(** Recursively replace every occurence of type synonyms with their base type *)

val anti_unify : 'a Ty.ty -> 'b Ty.ty -> 'a Ty.ty
(** Anti-unification for type patterns.

The anti-unifier of two types is the most specific type that is more general than both types.
For instance, the anti-unifier of [int list * string], [char list * string option] is ['a list * 'b]
where ['a] and ['b] are fresh type variables.

The function {!anti_unify} works on type
codes. Correspondingly, the anti-unifier of the previous
example [Pair (List Int, String)] and [Pair (List Char,
Option String)] is [Pair (List Any, Any)].

More formally, the anti-unifier of two type codes is inductively defined by:

anti_unify (C (x0,..,xn), D (y0,..,ym)) =
  if C == D and n == m then C (anti_unify x0 y0, ..., anti_unify xn ym)
  else Any

*)
