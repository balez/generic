(** Sum of Product view
    replicating the Haskell LIGD library

    Cheney, James, & Hinze, Ralf. (2002). A lightweight
    implementation of generics and dynamics. Pages 90--104 of
    Haskell Workshop02.

*)

open Generic_core
open Ty.T
open Generic_util
open Sum

type 'a sp =
  | Base  : 'a ty -> 'a sp (* base cases (char, int, float, ..) *)
  | Empty : empty sp
  | Sum   : 'a sp * 'b sp -> ('a, 'b) sum sp
  | Unit  : unit sp
  | Prod  : 'a sp * 'b sp -> ('a * 'b) sp
  | Delay : 'a ty -> 'a sp (* to break the recursion *)
  | Con   : string * 'a sp -> 'a sp
  | Field : string * 'a sp -> 'a sp
  | Iso   : 'a sp * ('a, 'b) Fun.iso -> 'b sp

val sumprod : 'a ty -> 'a sp

(** view = sumprod *)
val view : 'a ty -> 'a sp
