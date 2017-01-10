(** Functions that directly manipulate the memory representation of ocaml values.

    Unless marked otherwise, all functions are safe in the sense that
    they will not cause a segfault. Please report a bug otherwise.
*)

(** {2 General functions on caml values}
*)

val dup_if_block : Obj.t -> Obj.t
(** Duplicate a block. The identity on integers
 *)

(** {2 Value representation}
*)

(** Each ocaml values is associated to a tag, an integer. We provide a
    variant type {!tag} that is more descriptive.
*)
type tag =
  | Constructor of int
  | Lazy | Closure | Object | Infix | Forward
  | Abstract | String | Double | Double_array | Custom
  | Unaligned | Out_of_heap | Int

(** Variant type reflecting the implementation of ocaml
    values, they are either an immediate (integer) value or a
    block with a tag and a number of fields.
*)
type obj =
  | Int of int
  | Block of tag * Obj.t array

val view : Obj.t -> obj
(** A view on the memory representation of values. *)

val tag_view : int -> tag
(** Converts the result of [Obj.tag] to a more descriptive type.
    @raise Invalid_argument when the integer is not one of the known tag.
 *)

(** {2 Variant types} *)

type con_id = bool * int
(** Type of constructor identifiers, the result type of function {!val:con_id}
*)

val con_id : 'a -> con_id
(** Discriminates each constructor of a variant datatype by
    returning a distinct pair of (bool,int) for each of them.
    The boolean is true iff the constructor is constant.
    Two constructors of different types might have the same [con_id].
    The function doesn't discriminate the constructors of
    extensible variants.
*)

val is_con : Obj.t -> bool
(** True if the value could be a value of a non-polymorphic
    variant type: the value must be either an immediate value or
    a block whose tag is within the range of constructor tags.
*)

val gsize : Obj.t -> int
(** Either the size of a block
   (the number of its fields) or [0] for an immediate
   value.
*)

val fields_all2 : (Obj.t -> Obj.t -> bool) -> Obj.t -> Obj.t -> bool
(** [fields_all p x y] holds iff [x] and [y] are both be block of the same size,
and the binary predicate [p] holds for all their fields:

{v
fields_all2 p x y  <==>  forall i . p (x.i, y.i)
v}

*)

val listify : Obj.t -> Obj.t
(**  Given a obj of type [(x0,...xn)],
     computes a nested product [(x0, (x1, (..., xn) ...))]
 @raise Invalid_argument if the argument is not a tuple (a block with tag zero).
 *)

(** {2 Extensible variants}
    Helpers for working with the memory representation of extensible variants.
*)

val is_ext_con : Obj.t -> bool
(** Returns whether the object is a constructor of an extensible variant.
*)

val ext_con : Obj.t -> Obj.t
(** Extracts the constructor of an extensible variant
   (the from a value which may be a constructor application.
 @raise Invalid_argument when the value is not an extensible variant.

 {b Property.}  [is_ext_con (ext_con x)]
 *)

val ext_con_name : Obj.t -> string
(** Returns the constructor name of an extensible variant.
    @raise Invalid_argument when the value is not an extensile variant.
 *)

val ext_con_id : Obj.t -> int
(** Returns the unique object identifier associated with an extensible constructor.
    @raise Invalid_argument when the value is not an extensile variant.
 *)

(** {2 Polymorphic variants} *)

val poly_hash : 'a -> int
(** Returns the hash value associated with a constructor of a polymorphic variant
    @raise Invalid_argument if the argument is not a polymorphic variant
*)

(** {2 Custom blocks}
*)

val custom_identifier : 'a -> string
(** Returns the identifier of a custom block,
   or the empty string if the block isn't a custom one.
*)
