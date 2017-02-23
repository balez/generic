(** Namespace for modules defining generic functions *)

module Marshal = Generic_fun_marshal
(** Type-safe generic marshalling and unmarshalling operations. *)

module Equal = Generic_fun_equal
(** A generic equality that doesn't break abstract types. *)

module Uniplate = Generic_fun_uniplate
(** Boilerplate-less traversals for recursive types. *)

module Multiplate = Generic_fun_multiplate
(** Boilerplate-less traversals, generalising uniplate to
    mutually recursive types. *)
