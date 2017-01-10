(** Computing the low level description of types.

    The module {!module:Generic_core_desc} defines the datatype
    [desc] which is a low level generic view giving a detailed
    description of types.  The present module defines the
    extensible function {!view} that associates the
    description to each type. To extend [desc] to cover new
    types, the function {!ext} is used.

    All the types reflected in {!module:Generic_core_ty} are
    given a [desc] definition in this module.
*)

open Generic_core

(** Type of the generic view: a [desc_fun] takes a type witness ['a ty] and
    computes the description ['a desc] *)
type desc_fun =
 { f : 'a. 'a Ty.ty -> 'a Desc.t; }

(** Computes the generic view.
    @raise Type_pattern_match_failure if the type doesn't
    have a view yet. You can use {!ext} to extend [view]
    to cover those cases. *)
val view : 'a Ty.ty -> 'a Desc.t

(** [ext pattern new_case] extends [desc] with a [new_case] corresponding to the types that match [pattern].
    @raise Type_pattern_overwrite when trying to extend [desc] with a case that is already covered.
 *)
val ext : 'a Ty.pat -> desc_fun -> unit

(** {2 Extensible variants}
Since extensible variants are types that can be extended after being declared,
their description must necessarily be extensible as well. Therefore, to define the generic view for an extensible variant,
we will first declare it with {!ext_register}, then add its constructors
one by one using {!ext_add_con}. The functions [ext_{conap,iter,fold}] all act on extensible variants
and raise [Invalid_argument] in case the type isn't extensible.
 *)

(** Extensible variants are declared with this function.
@raise Pattern_overwrite the type already has a desc view.
 *)
val ext_register : 'a Ty.ty -> string -> unit

(* Retrieve the generic view associated with an extensible type.
   @raise Invalid_argument if the type is not an extensible type
val extensible : 'a Ty.ty -> 'a Desc.extensible *)

(** Adds a constructor to an extensible variant. *)
val ext_add_con : 'a Ty.ty -> Desc.Ext.con -> unit

(** Computes the constructor application view for a value of an extensible variant.
@raise Type_pattern_match_failure if the type isn't registered yet.
@raise Invalid_argument if the type is not an extensible type.
@raise Not_found if the constructor hasn't been added previously.
*)
val ext_conap : 'a Ty.ty -> 'a -> 'a Desc.Con.conap

(** [ext_iter t f] executes [f] on each constructor registered with the extensible variant witnessed by [t].
@raise Type_pattern_match_failure if the type isn't registered yet.
@raise Invalid_argument if the type is not an extensible type.
 *)
val ext_iter :'a Ty.ty -> ('a Desc.Con.t -> unit) -> unit

(** [ext_fold t] is similar to [List.fold_right] executed on the list of constructors of the extensible variant witnessed by [t].
@raise Type_pattern_match_failure if the type isn't registered yet.
@raise Invalid_argument if the type is not an extensible type.
 *)
val ext_fold : 'a Ty.ty -> ('a Desc.Con.t -> 'b -> 'b) -> 'b -> 'b
