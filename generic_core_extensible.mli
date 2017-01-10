(** Extensible type-indexed functions.

Type-indexed functions are ad-hoc polymorphic functions
rather than parametric polymorphic: their behaviour may
change depending on the type.

We implement them in Ocaml using a type ['a ty] (see {!Generic_core_ty.ty}) whose
constructors reflect the type parameter:

{[
Int : int ty
List : 'a ty -> 'a list ty
]}

The type [ty] is extensible to allow users to add their own types.

Therefore when defining functions on [ty] we need to be able
to extend them as we extend [ty]. This is the purpose of this
module.


The general type of type-indexed function is given by

{[
type 'f ty_fun = { f : 'a . 'a ty -> ('a,'f) app }
]}

where we used a defunctionalisation method to deal with
parameterised types, see {!module:Generic_util_app}.  The
idea is that [('a, 'f) app] represents the application of a
type ['f] to a type parameter ['a].

An extensible function is a collection of such [ty_fun].
A collection is created with function [create] whose result is a closure
of type ['b extensible_closure] whose field [f] is the extensible function,
and field [ext] allows to extend it to a new case by providing
a type pattern and a [ty_fun] matching that type pattern.

Calling the extensible function with the field [f] will raise
a [Type_pattern_match_failure] exception when the type index
doesn't match any of the patterns in the collection.

Extending an extensible function with the field [ext] will
raise a [Type_pattern_overwrite] exception when called with
a type pattern that was already registered.
*)

(* Re-exports *)
(* type 'a ty = 'a Generic_core_ty.ty = .. *)
(* type 'a pat = 'a Generic_core_ty.pat *)
(* type ('a, 'b) app = ('a, 'b) Generic_util_app.app = .. *)
open Generic_core
open Generic_util

exception Type_pattern_match_failure of string
(** Raised by the [f] field of a {!closure} when there is no
    case registered to deal with the type witness.
*)
exception Type_pattern_overwrite of string
(** Raised by the [ext] field of a {!closure} when trying to extend a type indexed function
    with a case for a pattern that is already covered.
*)

(** Type-indexed function, ['b] is the code for a parametric type ['a f]
such that ['b ty_fun] is equivalent to [for all 'a . 'a f]
*)
type 'b ty_fun = { f : 'a. 'a Ty.ty -> ('a, 'b) App.t; }


(** A closure allows us to call a type-indexed function and
    to extend it to new type cases.
*)
type 'b closure = {
    f : 'a. 'a Ty.ty -> ('a, 'b) App.t;
    (** [f]: applies the extensible function.
       @raise Type_pattern_match_failure when the type index doesn't
       match any of the patterns in the collection.  *)

    ext : 'a. 'a Ty.pat -> 'b ty_fun -> unit;
    (** [ext]: extends the function with a new case. We must
        provide a type pattern, (example: [List Any]). The
        [ty_fun] provided is only expected to handle types
        matching with the pattern. {b Effectful}.
        @raise Pattern_overwrite when called with a type pattern that
        was already registered.
    *)
  }

(** [create name], creates a new closure initially empty:
    calling [f] will raise
    [Type_pattern_match_failure]. The name is used in the exception messages. {b Effectful}. *)
val create : string -> 'f closure
