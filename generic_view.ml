(** Namespace for modules defining generic views.

    The modules in this namespace define a uniform (generic)
    view on which generic programs may be defined.
*)


module Spine = Generic_view_spine
(** The spine view shows the applicative structure of a value
    as a constructor applied to its arguments, it underlies the Scrap Your Boilerplate library for Haskell. *)

module Sumprod = Generic_view_sumprod
(** Sum of Product view
    inspired by Haskell LIGD library.
*)

module Conlist = Generic_view_conlist
(** List of constructors view, inspired by Haskell Replib library.
    Records and products are viewed as variants of one constructor.
*)
