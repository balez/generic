(** Extra functions on hashtables *)

open Generic_util

(** [safe_find], catches the [Not_found] exception with a
   default value:
{[
safe_find table key some none =
  try some (Hashtbl.find table key)
  with Not_found -> none
]}
*)
let safe_find table key some none =
  try some (Hashtbl.find table key)
  with Not_found -> none

(** Update a hashtable entry.
[update table key f x0] will either initialise [table[key]] with [f x0]
or update it with [f (table[key])].
*)
let update table key upd init_val =
  let new_val = upd (safe_find table key Fun.id init_val)
  in Hashtbl.replace table key new_val
