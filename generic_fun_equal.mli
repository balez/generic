open Generic_core
open Ty.T

module Sumprod : sig
  val equal : 'a ty -> 'a -> 'a -> bool
end

module Conlist : sig
  val equal : 'a ty -> 'a -> 'a -> bool
end
