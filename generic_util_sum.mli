(** Empty and Sum datatypes. *)

open Generic_core

(** {2 Types} *)
(** Type with zero element. *)
type empty

(** Sum type. [L] stands for {i left} and [R] stands for {i right}. *)
type ('a, 'b) choice = L of 'a | R of 'b

(** Type witnesses for [empty] and [choice]. *)
type _ Ty.t +=
    Empty : empty Ty.t
  | Choice : 'a Ty.t * 'b Ty.t -> ('a, 'b) choice Ty.t

(** {2 Operations} *)

(** Empty elimination: anything can be proved from a
    falsehood.  It shouldn't be possible to execute that function
    since we cannot give it any actual argument, however, the
    function is useful when pattern matching requires some
    impossible cases to be given anyway.
*)
val empty_elim : empty -> 'a
