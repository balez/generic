(** Safe generic marshalling and unmarshalling operations. *)

open Generic_core
open Ty.T

val to_channel : 'a ty -> out_channel -> 'a -> Marshal.extern_flags list -> unit
val to_string : 'a ty -> 'a -> Marshal.extern_flags list -> string
val to_bytes : 'a ty -> 'a -> Marshal.extern_flags list -> bytes
val from_channel : 'a ty -> in_channel -> 'a
val from_string : 'a ty -> string -> int -> 'a
val from_bytes : 'a ty -> bytes -> int -> 'a

(** Low level functions *)
val to_repr : 'a ty -> 'a -> Obj.t
val from_repr : 'a ty -> Obj.t -> 'a
