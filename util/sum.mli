(** Empty and Sum datatypes. *)
open Generic_core

(** {2 Types} *)
(** Type with zero element. *)
type empty

(** Sum type. *)
type ('a, 'b) sum = Left of 'a | Right of 'b

(** Type witnesses for [empty] and [sum]. *)
type _ Ty.t +=
    Empty : empty Ty.t
  | Sum : 'a Ty.t * 'b Ty.t -> ('a, 'b) sum Ty.t

val left : 'a -> ('a,'b) sum
val right : 'b -> ('a,'b) sum

(** {2 Operations} *)

(** Empty elimination: anything can be proved from a
    falsehood.  It shouldn't be possible to execute that function
    since we cannot give it any actual argument, however, the
    function is useful when pattern matching requires some
    impossible cases to be given anyway.
*)
val empty_elim : empty -> 'a
val either : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) sum -> 'c
val sum : ('a -> 'c) -> ('b -> 'd) -> ('a,'b) sum -> ('c,'d) sum
