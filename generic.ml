module Util = Generic_util
module Fun = Generic_fun

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

module Desc = struct
  include Generic_core_desc
  include Generic_core_desc_fun
end
