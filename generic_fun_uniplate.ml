(** Uniplate view

    The uniplate view represents recursive types as rose trees.

    It is inspired by the Haskell uniplate and multiplate libraries.

http://hackage.haskell.org/package/uniplate
http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm

*)

open Generic_core
open Generic_util

open Ty.T
open Desc.T
let desc = Desc_fun.view
