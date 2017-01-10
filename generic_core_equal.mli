(** Type equality predicate. *)

(** [x : ('a,'b) equal] is a witness that 'a and 'b are intensionally equal.

    Pattern matching [x] with [Refl] will force the type checker to unify ['a] and ['b].
 *)
type (_, _) equal = Refl : ('a, 'a) equal
type ('a,'b) t = ('a,'b) equal
