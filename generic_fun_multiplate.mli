(** Library for boilerplate-less traversals, generalising {!module:Generic_fun_uniplate} to
    mutually recursive types.

    The library is inspired by the Haskell multiplate library by Russell O'Connor.

- {{:http://hackage.haskell.org/package/multiplate}http://hackage.haskell.org/package/multiplate}
- {{:https://arxiv.org/pdf/1103.2841v1.pdf} Functor is to Lens as Applicative is to Biplate---Introducing Multiplate}

 *)

open Generic_core
open Generic_util
open Ty.T
open Ty.Dyn
open App.T
open Product.T

type 'f plate = {plate : 'a . 'a ty -> 'a -> ('a,'f) app}
(** A plate is a type-indexed function that may transform values of any type
while carrying some effect in an applicative functor or a monad.
*)

type id_plate = {id_plate : 'a . 'a ty -> 'a -> 'a}
(** Specialisation of [plate] to the identity functor.
*)

type 'b const_plate = {const_plate : 'a . 'a ty -> 'a -> 'b}
(** Specialisation of [plate] to a constant functor.
*)

val pure_plate : 'f applicative -> 'f plate
(** A plate that lift all values into the applicative functor.
For all [f], [t] and [x], [(pure_plate f).plate t x == f.pure x]
*)

val pure_id_plate : id_plate
(** Specialisation of [pure_plate] to the identity functor.
For all [t] and [x], [pure_id_plate.plate t x == x]
*)

val pure_const_plate : 't monoid -> 't const_plate
(** Specialisation of [pure_plate] to the constant functor.
For all [m], [t] and [x], [pure_const_plate.plate t x == m.mempty]
*)

val compose : 'f functorial -> 'g plate -> 'f plate -> ('g, 'f) App.comp plate
val compose_monad : 'f monad -> 'f plate -> 'f plate -> 'f plate
val compose_right_id : 'f plate -> id_plate -> 'f plate
val compose_left_id : 'f functorial -> id_plate -> 'f plate -> 'f plate
val append_plate : 'r monoid -> 'r const_plate -> 'r const_plate -> 'r const_plate

val traverse : 'f applicative -> 'f plate -> 'a product -> 'a -> ('a, 'f) app
(** Traversing a product type with effects *)

val map : id_plate -> 'a product -> 'a -> 'a
(** Mapping a function on each component of a product. [map] is
    the specialisation of [traverse] with the identity
    functor. *)

type 'a scrapped =
  Scrapped : 'b Product.t * 'b * ('b -> 'a) -> 'a scrapped
(**
  ['a scrapped] is meant to deconstruct the root of a tree into a tuple of subtrees (the children)
and a function that replace those children.
*)


val scrap : 'a ty -> 'a -> 'a scrapped
(** [scrap] is a generic function that deconstruct a value in
    a tuple children with a function to rebuild the value given
    the children.
*)

val children : 'a ty -> 'a -> dyn list
val children_d : dyn -> dyn list
(** [children] is a generic function that computes a list of
    dynamic values which are the immediate children of the given
    value.
*)

val family : 'a ty -> 'a -> dyn list
val family_d : dyn -> dyn list
(** [family] is a generic function that computes a list of
    dynamic values which are the descendent of a given value, that is:
    the value itself and the descendents of its immediate children
*)

val traverse_children_p : 'f applicative -> 'f plate -> 'f plate
val traverse_children : 'f applicative -> 'f plate -> 'a ty -> 'a -> ('a, 'f) app
(** Replace each child using the plate.
    (left to right traversal of children).
Note that [fmap_children] corresponds to [multiplate] and [mapChildrenM] in the Haskell library.
*)

val map_children_p : id_plate -> id_plate
val map_children : id_plate -> 'a ty -> 'a -> 'a
(** Specialisation of [fmap_children] with the identity functor.
Replace each child using the pure plate. *)

val traverse_family_p : 'f monad -> 'f plate -> 'f plate
val traverse_family : 'f monad -> 'f plate -> 'a ty -> 'a -> ('a, 'f) app
(** Bottom up (Depth-first, post-order) traversal of a value of a recursive type.

    Given a plate whose fields transform each type, [fmap_family_p]
    returns a plate whose fields transform the family of the
    input. The traversal proceeds bottom up, first transforming
    the families of the children, before finally transforming the
    value itself.
*)

val map_family_p : id_plate -> id_plate
val map_family : id_plate -> 'a ty -> 'a -> 'a
(** Specialisation of [fmap_family] with the identity monad.
*)

val para_p : ('r list -> 'r) const_plate -> 'r const_plate
val para_d : (dyn -> 'r list -> 'r) -> (dyn -> 'r)
val para : (dyn -> 'r list -> 'r) -> 'a ty -> 'a -> 'r
(* Paramorphism *)

val fold_children_p : 't monoid -> 't const_plate -> 't const_plate
val fold_children_d : 't monoid -> (dyn -> 't) -> (dyn -> 't)
val fold_children : 't monoid -> (dyn -> 't) -> 'a ty -> 'a -> 't
(** Use the plate on each child to get an element of the
    monoid, and use the monoid to reduce them to a single
    value. (left to right traversal of children).
    Corresponds to [mChildren] in the Haskell library.
*)

val pre_fold_p : 't monoid -> 't const_plate -> 't const_plate
val pre_fold_d : 't monoid -> (dyn -> 't) -> (dyn -> 't)
val pre_fold : 't monoid -> (dyn -> 't) -> 'a ty -> 'a -> 't
(** Folds a family in pre-order.

    Given a plate whose fields all return a Monoid o,
    preorderFold produces a plate that returns the mconcat of the
    family of the input.

    The input itself produces the leftmost element of the
    concatenation, then this is followed by the family of the
    first child, then it is followed by the family of the second
    child, and so forth.
*)

val post_fold_p : 't monoid -> 't const_plate -> 't const_plate
val post_fold_d : 't monoid -> (dyn -> 't) -> (dyn -> 't)
val post_fold : 't monoid -> (dyn -> 't) -> 'a ty -> 'a -> 't
(** folds a family in post-order

   Given a plate whose fields all return a Monoid o,
   preorderFold produces a plate that returns the mconcat of
   the family of the input.

   The concatenation sequence begins with the family of the
   first child, then it is followed by the family of the second
   child, and so forth until finally the input itself produces
   the rightmost element of the concatenation.
*)

(* TODO:
val breadth_fold_p : 't monoid -> 't const_plate -> 't const_plate
(** folds a family in breadth first order:
    The concatenation sequence begins with the input, then its children in left to right order,
    then the grand-children in left to right order,
    then the grand-grand-children, etc.
*)
*)


(* TODO: lazy family *)
