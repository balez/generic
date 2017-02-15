open Generic_core
open Generic_util
open Ty.T
open App.T

type 'f plate = {plate : 'a . 'a ty -> 'a -> ('a,'f) app}
type id_plate = {id_plate : 'a . 'a ty -> 'a -> 'a}
type 'b const_plate = {const_plate : 'a . 'a ty -> 'a -> 'b}

val pure_plate : 'f applicative -> 'f plate
val pure_id_plate : id_plate
val pure_const_plate : 't monoid -> 't const_plate

val compose : 'f functorial -> 'g plate -> 'f plate -> ('g, 'f) App.comp plate
val compose_monad : 'f monad -> 'f plate -> 'f plate -> 'f plate
val compose_right_id : 'f plate -> id_plate -> 'f plate
val compose_left_id : 'f functorial -> id_plate -> 'f plate -> 'f plate
val append_plate : 'r monoid -> 'r const_plate -> 'r const_plate -> 'r const_plate

type 'a scrapped =
  Scrapped : 'b Product.t * 'b * ('b -> 'a) -> 'a scrapped

val scrap : 'a ty -> 'a -> 'a scrapped

val children : 'a ty -> 'a -> Ty.dyn list

(** Replace each child using the plate.
    (left to right traversal of children).
Note that [fmap_children] corresponds to [multiplate] and [mapChildrenM] in the Haskell library.
*)
val fmap_children_p : 'f applicative -> 'f plate -> 'f plate
val fmap_children : 'f applicative -> 'f plate -> 'a ty -> 'a -> ('a, 'f) app

(** Replace each child using the pure plate. *)
val map_children_p : id_plate -> id_plate
val map_children : id_plate -> 'a ty -> 'a -> 'a

(** Bottom up (Depth-first, post-order) traversal of a value of a recursive type. The
    given function is applied to each member of the family.
*)
val fmap_family_p : 'f monad -> 'f plate -> 'f plate
val fmap_family : 'f monad -> 'f plate -> 'a ty -> 'a -> ('a, 'f) app

val map_family_p : id_plate -> id_plate
val map_family : id_plate -> 'a ty -> 'a -> 'a

(** Use the plate on each child to get an element of the
    monoid, and use the monoid to reduce them to a single
    value. (left to right traversal of children).
    Corresponds to [mChildren] in the Haskell library.
*)
val fold_children_p : 't monoid -> 't const_plate -> 't const_plate
val fold_children : 't monoid -> 't const_plate -> 'a ty ->  'a -> 't

(* folds a family in pre-order *)
val pre_fold_p : 't monoid -> 't const_plate -> 't const_plate
val pre_fold : 't monoid -> 't const_plate -> 'a ty -> 'a -> 't

(* folds a family in post-order *)
val post_fold_p : 't monoid -> 't const_plate -> 't const_plate
val post_fold : 't monoid -> 't const_plate -> 'a ty -> 'a -> 't

(* val map_family : 'a ty -> id_plate -> ('a -> 'a)*)
