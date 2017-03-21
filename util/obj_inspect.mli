(** Display the memory representation of OCaml values. *)

val show_obj : 'a -> string
(** @return the memory representation of a value. *)

val print_obj : 'a -> unit
(** Print the representation to standard output. *)
