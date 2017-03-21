(* Extensible type-indexed functions.

**************************************************
** Description

Type-indexed functions are ad-hoc polymorphic functions
rather than parametric polymorphic: their behaviour may
change depending on the type.

We implement them in Ocaml using a type "'a ty" whose
constructors reflect the type parameter:

    Int : int ty
    List : 'a ty -> 'a list ty

The type "ty" is extensible to allow users to add their own types.

Therefore when defining functions on "ty" we need to be able
to extend them as we extend "ty". This is the purpose of this
module.

The general type of type-indexed function is given by

    type 'f ty_fun =
      { f : 'a . 'a ty -> ('a,'f) app }

where we used a defunctionalisation method to deal with
parameterised types. "app" is defined in "App".  The
idea is that "('a, 'f) app" represents the application of a
type 'f to a type parameter 'a.

An extensible function is a collection of such ty_funs.
A collection is created with function "create" whose result
is a closure of type "'b Generic_core_extensible.closure" whose field "f" is
the extensible function, and field "ext" allows to extend it
to a new case by providing a type pattern and a ty_fun
matching that type pattern.

**************************************************
** Implementation

For fast application, we store the ty_funs in a hashtable
indexed by a type pattern, as defined in "Ty_patterns", the
keys of the hashtable are constructor patterns where all the
parameters of a type constructor are set to "Any", a
universal type pattern (a wildcard). For instance, all the
list cases of a type indexed function will be stored in the
hashtable entry associated with the constructor pattern "List
Any" ; and all functions on pairs will be associated with
pattern "Pair (Any,Any)", and so on.

For each constructor pattern we store a list of functions
ordered by their type pattern so that when applying the
extensible function to some given type, the most general
patterns are tried after all the more nested patterns have
failed to match the type. This mechanism allows us to add
specific cases to an extensible function in any order we
want, the behaviour at the end will be the same.  Note that
the order on pattern is lexicographical: so that (Pair
(Int,Any)) matches before (Pair (Any, Int)) also Pair
(Int,Int) matches before both of them, and Pair (Any,Any)
matches after all of them, and Any matches after that.

Using a constructor "Any : a ty" to represent a wild card
patterns, we register each function to a type constructor
pattern such that all the type parameters are "Any".
eg: "List Any", "Pair (Any,Any)" and so on.

The ordering on patterns is itself defined as an extensible
function in the module "Ty_patterns".

This approach combines both efficient application in the most
frequent case in which there will be only one definition per
constructor, and flexibility as it allows nested pattern
matching and the order in which the function is extended
doesn't matter.
 *)

open Generic_core
open Generic_util

exception Type_pattern_match_failure of string
exception Type_pattern_overwrite of string

type 'b ty_fun = { f : 'a. 'a Ty.ty -> ('a, 'b) App.t; }
type 'b closure = {

    (* f: applies the extensible function, raises
       Type_pattern_match_failure when the type index doesn't
       match any of the patterns in the collection.  *)

    f : 'a. 'a Ty.ty -> ('a, 'b) App.t;

    (* ext: extends the function with a new case, raises
      Pattern_overwrite when called with a type pattern that
      was already registered.
     *)

    ext : 'a. 'a Ty.pat -> 'b ty_fun -> unit; (* Effectful *)
  }


type 'f ty_table = (Ty.ty', (Ty.ty' * 'f ty_fun) list) Hashtbl.t

(* "apply_fun name table type"
       tries to get the table "entry" corresponding to "type"
       if not, then it tries the "Any" entry which is the generic case.
       then with the list of functions that we should have found,
       we try them in turn as long as they raise a Match_failure.
 *)

let apply_fun name h t =
  let apply_list fs =
    Listx.match_list (fun (_,(f : 'f ty_fun)) -> f.f t) fs
  in
  let default () =
    try Hashtbl.find h (Ty.E Ty.Any)
    with Not_found ->
      raise (Type_pattern_match_failure name)
  in
  try apply_list (Hashtbl.find h (Ty.E (Ty.conpat t)))
  with Not_found | Listx.Match_list_failure
                   -> try apply_list (default ())
                      with Listx.Match_list_failure
                           -> raise (Type_pattern_match_failure name)

let insert_fun name table t f =
  (*  preorder *)
  let fun_leq (Ty.E t, f) (Ty.E t', f') = Patterns.leq t t'
  in
  let insert xs x =
    try Listx.sl_insert fun_leq xs x
    with Listx.Insert_duplicate ->
      raise (Type_pattern_overwrite name)
  in
  Hash.update table (Ty.E (Ty.conpat t)) (insert (E t, f)) []

let create name =
  let table = Hashtbl.create 10
  in { f   = (fun t -> apply_fun name table t)
     ; ext = (fun t f -> insert_fun name table t f)
     }
