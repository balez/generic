(* * Generic_core_ty: Type reflection

In this module, we define the type =_ ty= reflecting type terms to value terms.
We call such terms /type codes/.
The GADT syntax allows us to constrain the type parameter of =_ ty= so that for any given
type parameter =t=, there is at most one value of type =t ty=, that value is the reflection of =t=.

For each type of arity =n=, corresponds a =ty= constructor of arity =n= whose arguments are themselves
=ty= values reflecting the type parameters.

*)

module T = struct
(* ** Type Reflection
=_ ty= is extensible: when the user defines a new type =('a0,...'an) t=,
we add a homonymous constructor to =_ty=:
: _ ty += T : 'a0 ty * ... * 'an ty -> ('a0,...'an) t ty
*)
  type _ ty = ..

(* *** Primitive and Standard Types
The core generic library comes with the type reflection and introspection for all types in the standard library. Other types need to be added by the user, either manually or through dedicated PPX.
*)
    type _ ty +=
     | Any    : 'a ty (* wildcard pattern *)
     (* Base types *)
     | Int32  : int32 ty
     | Int64  : int64 ty
     | Nativeint : nativeint ty
     | Lazy   : 'a ty -> 'a Lazy.t ty
     | Exn    : exn ty
     | Bool   : bool ty
     | Int    : int ty
     | Float  : float ty
     | Char   : char ty
     | Bytes  : bytes ty
     | String : string ty
     | Option : 'a ty -> 'a option ty
     | List   : 'a ty -> 'a list ty
     | Array  : 'a ty -> 'a array ty
     | Ref    : 'a ty -> 'a ref ty
     | Ty     : 'a ty -> 'a ty ty
     | Unit   : unit ty
     | Pair   : 'a ty * 'b ty -> ('a * 'b) ty
     | Triple   : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
     | Quadruple : 'a ty * 'b ty * 'c ty * 'd ty -> ('a * 'b * 'c * 'd) ty
     | Quintuple : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty -> ('a * 'b * 'c * 'd * 'e) ty
     | Sextuple  : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty -> ('a * 'b * 'c * 'd * 'e * 'f) ty
     | Septuple  : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) ty
     | Octuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) ty
     | Nonuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty * 'i ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) ty
     | Decuple   : 'a ty * 'b ty * 'c ty * 'd ty * 'e ty * 'f ty * 'g ty * 'h ty * 'i ty * 'j ty -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) ty
     | Fun    : 'a ty * 'b ty -> ('a -> 'b) ty
end

include T
type 'a t = 'a ty

(* ** Forgetting the index
The GADT syntax allows us to forget the type index using existential quantification.
The type =ty'= is the union of all ='a ty= for all ='a=.
*)
type ty' = E : 'a ty -> ty'

(* *** Equality on types of possibly different indices
Using the index-less =ty'= we may now compare ='a ty= and ='b ty= values
using the generic equality primitive.
*)
let eq x y = try E x = E y with _ -> false
let neq x y = not (eq x y)

(* ** Helping functions
=with_type= is used to help the type-checker fix the type of a value to the index of ='a ty=.
 *)
let with_type : 'a ty -> 'a -> 'a
  = fun _ x -> x

(* ** Type Patterns
*)
type 'a pat = 'a ty (* used in a context where Any is allowed *)

(* *** Wildcard pattern
The constructor =Any= is a type pattern that can match any type.
It is not valid in contexts where a type code is expected: use only in contexts where a pattern is expected.
*)

let any = Any

(* *** Some Patterns
*)
let pair = T.Pair (Any, Any)
let option = T.Option Any
let list = T.List Any
let array = T.Array Any


(* *** Constructor pattern
=conpat= computes the constructor pattern of a type code:
it replaces all the parameters with =Any=.
It uses the =Obj= representation to directly manipulate the
memory representation.
It generalises of the following cases:

: let conpat : type a . a ty -> a ty =
:   function
:   | Int -> Int
:   | List t -> List Any
:   | Pair (a,b) -> Pair (Any, Any)

*)
let conpat : 'a ty -> 'a ty =
  fun value ->
  let open Obj in
  let rv = repr value in
  tag rv |>
    function
    | t when t == object_tag
      (* [value] is a constructor of arity 0 *)
      -> value
    | t when t == 0
      (* [value] has a constructor [field rv 0] of arity [size rv - 1] *)
      -> let any = repr Any
         and n = size rv in
         let con = new_block t n in
         begin
           set_field con 0 (field rv 0);
           for i = 1 to n - 1 do
             set_field con i any
           done;
           obj con
         end
    | _ -> assert false
;;

(* =conpat'= is the =ty'= version of =conpat=, working on index-less type codes *)
let conpat' (E t) = E (conpat t)

(* * Dynamic values
*)

type 'a typed = 'a ty * 'a

type dyn = Dyn : 'a typed -> dyn
