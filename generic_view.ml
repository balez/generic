(** Namespace for modules defining generic views.

    The modules in this namespace define a uniform (generic)
    view on which generic programs may be defined.
*)

(** The spine view shows the applicative structure of a value
    as a constructor applied to its arguments, it underlies the Scrap Your Boilerplate library for Haskell. *)
module Spine = Generic_view_spine
