(** Functions on type patterns. *)
open Generic_core
open Ty.T

val leq : 'a ty -> 'b ty -> bool
(** Partial lexical order on type patterns used to implement
extensible functions allowing nested patterns of any depths.

- [Any <= x]
- [f (x0,..xn) <= f (y0,..yn)]
    iff  [x0 < y0 or (x0 == y0 and x1 < x2 ...)]
*)

val no_free_var : 'a ty -> bool
(** [true] iff the pattern doesn't contain [Any], and thus determines a specific type *)

val no_free_var' : Ty.ty' -> bool
(** [no_free_var' (E t) == no_free_var' t] *)
