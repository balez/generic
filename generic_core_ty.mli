(** Type witness.

The type [_ ty] reflects type terms as value terms.
We call such terms /type codes/.
The GADT syntax allows us to constrain the type parameter of [_ ty] so that for any given
type [t], there is at most one value of type [t ty], that value is the reflection of [t].

For each type of arity [n], corresponds a [ty] constructor of arity [n] whose arguments are themselves
[ty] values reflecting the type parameters.

*)

(** {2 Type Reflection}
[_ ty] is extensible: when the user defines a new type [('a0,...'an) t],
we add a homonymous constructor to [_ty]:
{[
_ ty += T : 'a0 ty * ... * 'an ty -> ('a0,...'an) t ty
]}
 *)

(** {3 Primitive and Standard Types}
The core generic library comes with the type reflection and introspection for all types in the standard library. Other types need to be added by the user, either manually or through dedicated PPX.
 *)
module T : sig
  type _ ty = ..
  type _ ty +=
    | Any : 'a ty
    | Int32 : int32 ty
    | Int64 : int64 ty
    | Nativeint : nativeint ty
    | Lazy : 'a ty -> 'a Lazy.t ty
    | Exn : exn ty
    | Bool : bool ty
    | Int : int ty
    | Float : float ty
    | Char : char ty
    | Bytes : bytes ty
    | String : string ty
    | Option : 'a ty -> 'a option ty
    | List : 'a ty -> 'a list ty
    | Array : 'a ty -> 'a array ty
    | Ref : 'a ty -> 'a ref ty
    | Ty : 'a ty -> 'a ty ty
    | Unit : unit ty
    | Pair : 'a ty * 'b ty -> ('a * 'b) ty
    | Triple   : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
    | Quadruple : 'a ty * 'b ty * 'c ty * 'd ty -> ('a * 'b * 'c * 'd) ty
    | Quintuple : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty -> ('a * 'b * 'c * 'd * 'e) ty
    | Sextuple  : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty -> ('a * 'b * 'c * 'd * 'e * 'f) ty
    | Septuple  : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) ty
    | Octuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) ty
    | Nonuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty * 'i ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) ty
    | Decuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty * 'i ty * 'j ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) ty
    | Fun : 'a ty * 'b ty -> ('a -> 'b) ty
end
(** Synonym for convenience. *)
type 'a t = 'a T.ty
type 'a ty = 'a T.ty = ..

(** Re-export of T.ty constructors *)
type _ ty +=
   | Any : 'a ty
   | Int32 : int32 ty
   | Int64 : int64 ty
   | Nativeint : nativeint ty
   | Lazy : 'a ty -> 'a Lazy.t ty
   | Exn : exn ty
   | Bool : bool ty
   | Int : int ty
   | Float : float ty
   | Char : char ty
   | Bytes : bytes ty
   | String : string ty
   | Option : 'a ty -> 'a option ty
   | List : 'a ty -> 'a list ty
   | Array : 'a ty -> 'a array ty
   | Ref : 'a ty -> 'a ref ty
   | Ty : 'a ty -> 'a ty ty
   | Unit : unit ty
   | Pair : 'a ty * 'b ty -> ('a * 'b) ty
   | Triple   : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
   | Quadruple : 'a ty * 'b ty * 'c ty * 'd ty -> ('a * 'b * 'c * 'd) ty
   | Quintuple : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty -> ('a * 'b * 'c * 'd * 'e) ty
   | Sextuple  : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty -> ('a * 'b * 'c * 'd * 'e * 'f) ty
   | Septuple  : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) ty
   | Octuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) ty
   | Nonuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty * 'i ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) ty
   | Decuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty * 'i ty * 'j ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) ty
   | Fun : 'a ty * 'b ty -> ('a -> 'b) ty

(** {3 Forgetting the index}
GADT allow us to forget the type index using existential quantification.
The type [ty'] is the union of all ['a ty] for all ['a].
 *)

type ty' = E : 'a ty -> ty'

(** {3 Equality on types of possibly different indices}
Using the index-less [ty'] we may now compare ['a ty] and ['b ty] values
using the generic equality primitive.
 *)

val eq : 'a ty -> 'b ty -> bool
(** eq x y == E x = E y *)

val neq : 'a ty -> 'b ty -> bool
(** neq x y == not (eq x y) *)

(** {3 Functions for constraining a type } *)

val with_type : 'a ty -> 'a -> 'a
(** [with_type] is used to help the type-checker fix the type of a value to the index of ='a ty=. *)

(** {2 Type Patterns}

The type synonym ['a pat] is meant to mark the contexts
where patterns are expected rather than type codes. Any type code
is also a type pattern, but in addition there is a wildcard
pattern {!Any} which is not a type code.
*)

type 'a pat = 'a ty

(** {3 Wildcard Pattern}
The constructor [Any] is a type pattern that can match any type.
It is not valid in contexts where a type code is expected: use only in contexts where a pattern is expected.
 *)

val any : 'a pat
(** any == Any *)

(** {3 Patterns Functions}
Patterns for common type constructors are given.
 *)

val pair : ('a * 'b) pat
(** pair == Pair (Any,Any) *)

val option : 'a option pat
(** option == Option Any *)

val list : 'a list pat
(** list == List Any *)

val array : 'a array pat
(** array == Array Any *)

(** {3 Constructor Pattern}
[conpat] computes the constructor pattern of a type code:
it replaces all the parameters with [Any].

For instance:

{[
conpat Int == Int
conpat (List Int) == List Any
conpat (Pair (String, List Int)) == Pair (Any, Any)
]}
*)

val conpat : 'a ty -> 'a pat
(** [conpat] computes a constructor pattern:
{[
conpat (Pair (Int, List String)) == Pair (Any,Any)
]} *)

val conpat' : ty' -> ty'
(** [conpat'] is the [ty'] version of [conpat], working on index-less type codes.
{[
conpat' (E t) == E (conpat t)
]}
 *)

(** {2 Dynamic values}

Dynamically typed languages have a notion of runtime types which we can
emulate by packaging type witnesses with the values.
*)

(** Values tagged with their type witness. *)
type 'a typed = 'a ty * 'a

(** Dynamic values are the union of all types.

Using type-tagged values allows us to recover the type of the
value by pattern matching on the type witness.
*)
type dyn = Dyn : 'a typed -> dyn
