(** Library for boilerplate-less tree-traversals.

    It is inspired by the Haskell uniplate by Neil Mitchell.

- {{:http://hackage.haskell.org/package/uniplate}http://hackage.haskell.org/package/uniplate}
- {{:http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm}tutorial}

    The function names, implementation and many comments are
    taken from
    {{:http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html}Traversal}
    implementation by Sebastian Fischer, and are the same as in
    the {!module:Generic_fun_multiplate} generalisation.
*)

open Generic_core
open Generic_util
open Ty.T
open App.T

val scrap : 'a ty -> 'a -> 'a list * ('a list -> 'a)
(** [Scrap a] returns the list of children of type [a] of a value of type [a]
and a function to replace the children. That function should be given a list of the same size
as the children list.

{[
x == let (children, replace) = scrap a b x in replace children
]}
*)


(** The list of children of type [a] of a value of type [a] *)
val children : 'a ty -> 'a -> 'a list

(** Replacing the children of a value with new children. The
    list should contain exactly the number of required children.

    (Alternative specification: additional children are ignored,
    and if not enough are given, only the first ones are changed)

*)
val replace_children : 'a ty -> 'a -> 'a list -> 'a

(** Replace the children of a value using the given function. *)
val map_children : 'a ty -> ('a -> 'a) -> ('a -> 'a)

(** A family is a list of descendents.
A descendent of a value is either the value itself or a descendent of one of its children.
[family] builds a pre-order family where the value is listed before its descendents *)
val family : 'a ty -> 'a -> 'a list


(** Bottom up (Depth-first, post-order) traversal of a value of a recursive type. The
    given function is applied to each member of the family.
*)
val map_family : 'a ty -> ('a -> 'a) -> ('a -> 'a)

(** given a function that may optionally transform a value,
    [reduce_family] returns a function that exhaustively
    transforms the family of the input. The traversal proceeds
    bottom-up, first transforming the families of the
    children. If a transformation succeeds then the result is
    re-reduce_family-ed.

    A post-condition is that the input function returns
    [None] on all family members of the output.
*)
val reduce_family : 'a ty -> ('a -> 'a option) -> 'a -> 'a

(** Paramorphism. this is a bottom-up  *)
val para : 'a ty -> ('a -> 'r list -> 'r) -> 'a -> 'r

(** applicative and monadic variants *)

val traverse_children : 'f applicative -> 'a ty ->
  ('a -> ('a, 'f) app) -> ('a -> ('a, 'f) app)
(** applicative variant of {!map_children} *)

val traverse_family : 'f monad -> 'a ty ->
  ('a -> ('a, 'f) app) -> ('a -> ('a, 'f) app)
(** monadic variant of {!map_family} *)

val mreduce_family : 'f monad -> 'a ty ->
  ('a -> ('a option, 'f) app) -> 'a -> ('a, 'f) app
(** monadic variant of {!reduce_family} *)
