(** Useful functions on lists. *)

open Generic_util
open App.T

let (-<) = Fun.(-<)

(** [foldr c n l = List.fold_right c l n] *)
let foldr c n l = List.fold_right c l n

(** [cons] is useful when we want partial application of the list constructor. *)
let cons x xs = x :: xs

(** [from_to a b = [a; a+1; ... ; b]] *)
let rec from_to a b =
  if a > b then []
  else a :: from_to (a+1) b

(** [replicate n x = [x; x; ...; x]] with [x] repeated [n] times. *)
let rec replicate n x =
  if n <= 0 then [] else x :: replicate (n-1) x

(** [with_indices [x0; x1; ...; xn] = [(0,x0); (1,x1); ... ; (n, xn)]] *)
let with_indices xs =
  let rec go n = function
    | [] -> []
    | x :: xs -> (n,x) :: go (n+1) xs
  in go 0 xs


(** [find_index p xs] is the index of the first element of [xs] for which the predicate [p] is true:
[p (List.nth xs (find_index p xs)) == true]
and [(n < find_index p xs ==> p (List.nth xs n) == false]
@raise Not_found if no element verifies the predicate
*)
let find_index p = (* find_index p = fst -< List.find (p -< snd) -< with_indices *)
  let rec go n = function
    | [] -> raise Not_found
    | y :: ys -> if p y then n
      else go (n+1) ys
  in go 0

(** [index x xs] is the index of the first element of [xs] equal to [x].
[index x = find_index (fun y -> x = y)]
[List.nth xs (index x xs) = x]

Uses Pervasives.(=)
@raise Not_found if [x] is not an element of the list
 *)
let index x = find_index (fun y -> x = y)

(** [findOpt p xs] returns [Some x] if [x] is the first
    element of the list [xs] verifying the predicate [p]. or
    [None] if no element verifies the predicate. This
    function is total. *)
let find_opt p xs =
  try Some (List.find p xs)
  with Not_found -> None

(** [take_while p l] returns the longest prefix of [l] whose
    elements make [p] true.*)

let rec take_while p = function
  | [] -> []
  | h :: t ->
    if p h then h :: take_while p t else []

(** [drop_while p l] returns the suffix of [l] after removing
    the longest prefix of [l] whose elements make [p] true.
{[
take_while p l @ drop_while p l = l
]}
*)

let rec drop_while p = function
  | [] -> []
  | h :: t as l ->
    if p h then drop_while p t else l

(** [find_some f l] applies an option-valued function [f] to
    the elements of a list [l] and returns the first element that is not [None]
    or [None] if no such element exists.
*)

let find_some f xs =
  let ys = drop_while (fun x -> f x = None) xs
  in match ys with
  | [] -> None
  | x :: _ -> f x

(** [filter_some f l] applies [f] to every element [x] of the
    list [l] yielding either [None] which is thrown away, or
    [Some y] in which case [y] is kept.

{[
filter p = filter_some (fun x -> if p x then Some x else None)
]}
*)

let filter_some f xs =
  let unopt = function
      | None -> assert false
      | Some x -> x
  and some x = x <> None
  in let open List in
  map unopt (filter some (map f xs))

(** [set i x xs] computes a new list where the [i]-th element
    of [xs] is [x], the rest of the list is unchanged. Counting
    from [0].

    If [i] is greater than the length of the list or if [i]
    is negative, then the original list is returned.
*)
let set n x xs =
  let rec go n = function
    | [] -> []
    | y :: ys ->
      if n = 0
      then x :: ys
      else y :: go (n-1) ys
  in
  if n < 0 then xs else go n xs

(** {[concatmap f x = concat -< map f]}
    [concatmap] is the flipped bind operator of the list monad
 *)
let concatmap f xs = List.concat (List.map f xs)

type list' = LIST
type (_,_) app += List : 'a list -> ('a, list') app
let get_list = function
  | List x -> x
  | _ -> assert false


let monoid =
  { mempty = []
  ; mappend = (fun x y -> x @ y)
  }
let monad =
  { return = (fun x -> List [x])
  ; bind = (fun xs f -> List (concatmap (get_list -< f) (get_list xs)))
  }

  (** Raised by {!sl_insert} when trying to insert an element that is already in the list. *)
exception Insert_duplicate

(** [sl_insert leq x xs] inserts the new element [x] in the sorted list [xs] using the pre-order [<=].
    @raise Insert_duplicate when [x] is already in [xs].
 *)
let rec sl_insert leq x = function
  | [] -> [x]
  | y :: ys as yys ->  if leq x y then
                         if leq y x then raise Insert_duplicate
                         else x :: yys
                       else y :: sl_insert leq x ys


(** Raised by {!match_list}. *)
exception Match_list_failure

(** [match_list] tries to apply a function to the first
   element of a list. If a [Match_failure] is caught, we
   proceed with the rest of the list. If no more element is available (empty list),
   the exception [Match_list_failure] is raised.
   @raise Match_list_failure if the list is empty or if all the elements of the list
caused a [Match_failure].
 *)
let match_list f =
  let rec go = function
    | [] -> raise Match_list_failure
    | h :: t -> try f h
                with Match_failure (_,_,_) -> go t
  in go

let rec traverse a f = let open App.T in function
    | [] -> a.pure []
    | h :: t -> App.liftA2 a cons (f h) (traverse a f t)

let sequence a = traverse a (fun x -> x)
