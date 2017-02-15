(** Uniplate library for boilerplate-less tree-traversals

    The library is inspired by the Haskell uniplate library.

http://hackage.haskell.org/package/uniplate
http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm

The function names, implementation and many comments are taken from Traversal implementation.

http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html
*)

open Generic_core
open Ty.T

(** [Scrap a b] returns the list of children of type [b] of a value of type [a]
and a function to replace the children. That function should be given a list of the same size
as the children list.

{[
x == let (children, replace) = scrap a b x in replace children
]}
*)


val scrap : 'a ty -> 'b ty -> 'a -> 'b list * ('b list -> 'a)

(** The list of children of type [b] of a value of type [a] *)
val children : 'a ty -> 'b ty -> 'a -> 'b list

(** Replacing the children of a value with new children. The
    list should contain exactly the number of required children.

    (Alternative specification: additional children are ignored,
    and if not enough are given, only the first ones are changed)

*)
val replace_children : 'a ty -> 'b ty -> 'a -> 'b list -> 'a

(** Replace the children of a value using the given function. *)
val map_children : 'a ty -> 'b ty -> ('b -> 'b) -> ('a -> 'a)

(** A family is a list of descendents.
A descendent of a value is either the value itself or a descendent of one of its children.
[family] builds a pre-order family where the value is listed before its descendents *)
val family : 'a ty -> 'a -> 'a list

(** A post-order family where the value is listed after its descendents.
val post_family : 'a ty -> 'a -> 'a list*)

(** Concatenates the families of the children of a value *)
val child_families: 'a ty -> 'b ty -> 'a -> 'b list

(** Bottom up (Depth-first, post-order) traversal of a value of a recursive type. The
    given function is applied to each member of the family.
*)
val map_family : 'a ty -> ('a -> 'a) -> ('a -> 'a)

(** Applies the given function to each member of the families of the children
    of a value. The value and its children can have different types.
    Proceeds bottom-up.
*)
val map_child_families : 'a ty -> 'b ty -> ('b -> 'b) -> ('a -> 'a)

(** given a function that may optionally transform a value,
    [eval_family] returns a function that exhaustively
    transforms the family of the input. The traversal proceeds
    bottom-up, first transforming the families of the
    children. If a transformation succeeds then the result is
    re-eval_family-ed.

    A post-condition is that the input function returns
    [None] on all family members of the output.
*)
val eval_family : 'a ty -> ('a -> 'a option) -> 'a -> 'a


(** Executes [eval_family] on the children of a value:
{[eval_child_families a b f x = map_children a b (eval_family b f) x]}
*)
val eval_child_families : 'a ty -> 'b ty -> ('b -> 'b option) -> 'a -> 'a


(** Paramorphism. this is a bottom-up  *)
val fold : 'a ty -> ('a -> 'r list -> 'r) -> 'a -> 'r

(** Fold the children and combine the results. *)
val fold_children : 'a ty -> 'b ty -> ('a -> 'rb list -> 'ra) -> ('b -> 'rb list -> 'rb) -> 'a -> 'ra

(* TODO applicative and monadic variants *)
