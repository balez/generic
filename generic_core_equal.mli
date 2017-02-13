(** Type equality predicate. *)
open Generic_core

(** [x : ('a,'b) equal] is a witness that 'a and 'b are intensionally equal.

    Pattern matching [x] with [Refl] will force the type checker to unify ['a] and ['b].
 *)
type (_, _) equal = Refl : ('a, 'a) equal
type ('a,'b) t = ('a,'b) equal

type equal_fun =
  { f : 'a 'b . 'a Ty.t -> 'b Ty.t -> ('a, 'b) equal option }

(** extends [equal] with a new case. *)
val ext : 'a Ty.pat -> equal_fun -> unit

val equal : 'a Ty .t -> 'b Ty.t -> ('a,'b) equal option

(** Coerces a value between two intensionally equal types.
    In other words, [('a, 'b) equal] must be inhabited for a
    value of type ['a] to be coerced into the type ['b].
*)
val coerce : 'a Ty.t -> 'b Ty.t -> 'a -> 'b option
