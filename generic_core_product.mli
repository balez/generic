(** Witness of product types. *)

(** ['a product] is a GADT whose type parameter is of the form:
{[t0 * (t1 * (t2 * ... * (tn * unit) ...))]}

The values are lists of witnesses ['a ty] reflecting the
components of the product.
 *)

open Generic_core
open Ty.T

(** This module defines the [product] datatype and is meant
    to be open so that constructors can be used in pattern
    matching. *)
module T : sig
  type 'a product =
  | Nil : unit product
  | Cons : 'a ty * 'b product -> ('a * 'b) product
end

(** Synonym, so that [Product.T.product] might be refered to as [Product.t] *)
type 'a t = 'a T.product

(** {2 Product Constructors } *)
(** Functions [p0] to [p6] build products from 0 to 6 types. *)

val p0 : unit t
val p1 : 'a ty -> ('a * unit) t
val p2 :
  'a ty ->
  'b ty -> ('a * ('b * unit)) t
val p3 :
  'a ty ->
  'b ty ->
  'c ty -> ('a * ('b * ('c * unit))) t
val p4 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty -> ('a * ('b * ('c * ('d * unit)))) t
val p5 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty -> ('a * ('b * ('c * ('d * ('e * unit))))) t
val p6 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  ('a * ('b * ('c * ('d * ('e * ('f * unit)))))) t

(** [open Build] to bring [p0] to [p6] in scope *)
module Build : sig
  val p0 : unit t
  val p1 : 'a ty -> ('a * unit) t
  val p2 :
    'a ty ->
    'b ty -> ('a * ('b * unit)) t
  val p3 :
    'a ty ->
    'b ty ->
    'c ty -> ('a * ('b * ('c * unit))) t
  val p4 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty -> ('a * ('b * ('c * ('d * unit)))) t
  val p5 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    'e ty -> ('a * ('b * ('c * ('d * ('e * unit))))) t
  val p6 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    'e ty ->
    'f ty ->
    ('a * ('b * ('c * ('d * ('e * ('f * unit)))))) t
  val p7 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    'e ty ->
    'f ty ->
    'g ty ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * unit))))))) t
  val p8 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    'e ty ->
    'f ty ->
    'g ty ->
    'h ty ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * unit)))))))) t
  val p9 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    'e ty ->
    'f ty ->
    'g ty ->
    'h ty ->
    'i ty ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * unit))))))))) t
  val p10 :
    'a ty ->
    'b ty ->
    'c ty ->
    'd ty ->
    'e ty ->
    'f ty ->
    'g ty ->
    'h ty ->
    'i ty ->
    'j ty ->
    ('a * ('b * ('c * ('d * ('e * ('f * ('g * ('h * ('i * ('j * unit)))))))))) t

end

(** {2 Products as Lists}
If we forget about their type index, values of type products are isomorphic to lists of types.
Thus list functions can be specialised for products.
*)

val fold : (Ty.ty' -> 'b -> 'b) -> 'b -> 'a t -> 'b
(** Corresponds to [List.fold_right] *)

val length : 'a t -> int
(** Corresponds to [List.length] *)

val list_of_prod : 'a t -> Ty.ty' list
(** Forgetting the type index ['a], we obtain a list of unindexed type codes {!Generic_core_ty.ty'} *)

(** {2 Dynamic Products}

Just like {!Generic_core_ty.dyn} is the union of all types, we define
{!dynprod}, the union of all product types.  Each
value must be tagged with the witness of its product type.
 *)

(** A value tagged by the witness of its product type *)
type 'a tuple = 'a t * 'a

(** Union of all product types *)
type dynprod = Dynprod : 'a tuple -> dynprod

(** Dynamic products are isomorphic to lists of dynamic values *)
val list_of_dynprod : dynprod -> Ty.dyn list
val dynprod_of_list : Ty.dyn list -> dynprod
