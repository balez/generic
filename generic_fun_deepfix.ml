(** Extensible variants are implemented using object blocks,
    that get assigned a unique identifier at runtime.
    deserialization also generates a new identifier for every
    object block. If we want deserialized extensible variants
    to work properly with comparison and pattern matching, we
    need to fix their identifier, and revert it to the one
    they had before serialization.

    The function [Generic_core.Desc.Ext.fix] does that, but
    only for one constructor.  The present module implement a
    recursive function that fixes all the extensible
    constructors in a given value.  This is of course only
    possible if the types have been reified, and the [desc]
    view is defined for every extensible variant in the type.
*)

open Generic_core
open Ty.T
open Generic_fun

(** {!deepfix} recursively fixes the identifier of the
    constructors of extensible variant types in a value.
    Values of other types are left unchanged (and shared).
*)

let rec deepfix : type a . a ty -> a -> a
  = fun t x ->
    match Desc_fun.view t with
    | Desc.Extensible e ->
      (match Desc.Ext.conap e (Desc.Ext.fix e x) with
       | Desc.Con.Conap (c, y) ->
         c.embed (Multiplate.map {id_plate = deepfix} (Desc.Con.product c) y))
    | _ -> x
