(** Uniplate view

    The uniplate view represents recursive types as rose trees.

    It is inspired by the Haskell uniplate and multiplate libraries.

http://hackage.haskell.org/package/uniplate
http://hackage.haskell.org/package/multiplate-0.0.3/docs/Data-Generics-Multiplate.html
http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm

The terminology and many comments follow are taken from Traversal implementation.

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

(** A family is a list of a value and its children, the children of those children, etc *)
val family : 'a ty -> 'a -> 'a list

(** Concatenates the families of the children of a value *)
val child_families : 'a ty -> 'b ty -> 'a -> 'b list


(** Bottom up traversal of a value of a recursive type. The
    given function is applied to each member of the family
*)
val map_family : 'a ty -> ('a -> 'a) -> ('a -> 'a)

(** Applies the given function to each member of the families of the children
    of a value. The value and its children can have different types.
    Proceeds bottom-up.
*)
val map_child_families : 'a ty -> 'b ty -> ('b -> 'b) -> ('a -> 'a)

(** Recursively map the given function to each member of the family of a value until
    that function yields [None] for all members.
    Proceeds bottom-up.
*)
val recurse_family : 'a ty -> ('a -> 'a option) -> 'a -> 'a


(** Executes [recurse_family] on the children of a value:
{[recurse_child_families a b f = map_children a b (recurse_family b f)]}
*)
val recurse_child_families : 'a ty -> 'b ty -> ('b -> 'b option) -> 'a -> 'a


(** Paramorphism. *)
val fold : 'a ty -> ('a -> 'r list -> 'r) -> 'a -> 'r

(** Fold the children and combine the results. *)
val fold_children : 'a ty -> 'b ty -> ('a -> 'rb list -> 'ra) -> ('b -> 'rb list -> 'rb) -> 'a -> 'ra

(* TODO applicative and monadic variants *)
