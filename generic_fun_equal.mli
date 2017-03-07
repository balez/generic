open Generic_core
open Ty.T

module type Equal = sig
  val equal : 'a ty -> 'a -> 'a -> bool
end

module Sumprod : Equal
module Conlist : Equal
module Spine : Equal
