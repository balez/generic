(** Main Namespace for the library.

    {!Generic} is the union of the namespaces {!Util}, {!View}
    and {!Generic_core}. The modules Desc and Desc_fun are
    merged.  The modules from {!Generic_fun} are merged except
    {!Generic_fun_uniplate} and {!Generic_fun_multiplate}.
*)

(*
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

*)

include Generic_util
include Generic_view
(*include Generic_core*)

module Uniplate = Generic_fun_uniplate
(** Boilerplate-less traversals for recursive types. *)

module Multiplate = Generic_fun_multiplate
(** Boilerplate-less traversals, generalising uniplate to
      mutually recursive types. *)

module SafeMarshal = Generic_fun_marshal
(** Type-safe generic marshalling and unmarshalling operations. *)

(** Generic functions *)
module Gfun = struct
  open Generic_fun
  include Deepfix
  include Equal.Conlist
  include Show
end

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


(** Union of {!Generic_core_Desc} and {!Generic_core_Desc_fun} *)
module Desc = struct
  include Generic_core_desc
  include Generic_core_desc_fun
end
