(** Iterative functions. *)

(** [for_all_in a b f]  is the imperative equivalent of
    [fun a b f -> List.for_all f (from_to a b)]

(see {!Generic_util_list.from_to})
*)
val for_all_in : int -> int -> (int -> bool) -> bool
