(** Miscellaneous definitions. *)

(** [erase x] is used when only the side effects of a computation are needed, not its value.
{[erase x = ()]} *)
let erase _ = ()
