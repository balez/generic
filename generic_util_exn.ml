(** Operations on/with exceptions. *)

(** Indicates a feature that is yet to be implemented. *)
exception Not_yet_implemented

(** A function may raise [Undefined] outside of its domain of definition. *)
exception Undefined

(** Indicates a failure. Used by {!guard}, {!only_if}, {!one_of}, {!Generic_util_option.get_some}.
*)
exception Failed

(** [guard x] does nothing if [x] is true, but raises {!Failed} if [x] is false. *)
let guard x = if x then () else raise Failed

(** [only_if f x] returns [x] only if [f x] is true.
 @raise Failed when [f x] is false.
*)
let only_if f x = begin guard (f x); x end

(** [one_of xs] tries each action of the list [xs]
in turn until one succeeds (meaning that it does not raise {i any} exception).
@raise Failed If the action list is empty or if all the actions raised an exception.
 *)
let rec one_of = function
  | [] -> raise Failed
  | h::t -> try Lazy.force h
            with _ -> one_of t
