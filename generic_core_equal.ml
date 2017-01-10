(* type equality,
x : ('a,'b) equal is a witness that 'a and 'b are intensionally equal. *)
type (_,_) equal = Refl : ('a,'a) equal
type ('a,'b) t = ('a,'b) equal
