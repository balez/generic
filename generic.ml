(** Main Namespace for the library.

{!Generic} contains the {!Util}, {!View} and {!Fun} namespaces,
and re-exports the modules of the namespace {!Generic_core}.
*)

(** Namespace of utility modules. *)
module Util = Generic_util

(** Namespace for modules defining generic views. *)
module View = Generic_view

(** Namespace for modules defining generic functions. *)
module Gfun = Generic_fun

(** Type witness. *)
module Ty = Generic_core_ty

(** Generic view for the type witnesses. *)
module Ty_desc = Generic_core_ty_desc

(** Functions on type patterns. *)
module Patterns = Generic_core_patterns

(** Anti-unification for type patterns. *)
module Antiunify = Generic_core_antiunify

(** Representation of abstract datatypes. *)
module Repr = Generic_core_repr

(** Type equality predicate. *)
module Equal = Generic_core_equal

(** Witness of product types. *)
module Product = Generic_core_product

(** Extensible type-indexed functions. *)
module Extensible = Generic_core_extensible

(** Extensible type-indexed consumers. *)
module Consumer = Generic_core_consumer

(** Type description: a low level generic view of OCaml types.*)
module Desc = Generic_core_desc

(** Type indexed function that comptes the type description.*)
module Desc_fun = Generic_core_desc_fun
