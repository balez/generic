(** Spine view

    The spine view shows the applicative structure of a value
    as a constructor applied to its arguments.  The spine
    view is a generic view on a typed value. This is in
    contrast with most other views which show the structure
    of types. Consequently, this view can only be used to
    write generic consumer, not generators.

    The spine view underlies the Haskell SYB generic library
    (Scrap Your Boilerplate).
*)

open Generic_core
open Ty.T

(**
We call left spine the path from the root of a tree to its
leftmost leaf.  The spine generic view represent the possibly
partial spine of a tree whose node are applications, and
whose root is a data constructor.

The type is a GADT. The constructor [App] uses an
existential quantification for the type of the argument.  The
representation of the argument type is useful when defining
generic computations over a spine.
*)

module T : sig
  type 'a spine =
    | Con : 'a -> 'a spine
    | App : ('a -> 'b) spine * 'a ty * 'a -> 'b spine
end

open T

type 'a t = 'a spine

(**
A spine view converts a constructor value [c(v0,...vn)]
to the form [App (....(App (c', t0, v0)..., tn, vn)]
where [tk] is the representation of [vk]'s type.
and [c' = Con (c\_desc, fun x0 ... xn -> c(x0,...,xn)].

In essence, the spine view of a value gives a data
constructor and a list of all its arguments together with
their types.

<<spine view constructors for int lists>>=
let nil = Con (nild, [])
and cons ty hd tl =
  App (App (Con (cons_desc, fun x y -> x :: y), ty, hd)
      , List ty, tl)
@
*)


val spine : 'a ty -> 'a -> 'a spine

(** view = spine *)
val view : 'a ty -> 'a -> 'a spine

(** Builds back a value from it's spine view. *)
val rebuild : 'a spine -> 'a
