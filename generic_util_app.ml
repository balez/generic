(* Dealing with parametric types using defunctionalisation. *)

module T = struct
  type ('a,'b) app = ..
end

open T
type ('a,'b) t = ('a,'b) app

(* Example: the constant functor.

 The type [const] doesn't build useful values, we use it as a {i code}
 to be interpreted by [app] so that [('a, 'b const) app] is isomorphic to
['b]
 *)
type 't const = CONST
type (_, _) app += Const : 't -> ('a, 't const) app

let get_const = function
  | Const f -> f
  | _ -> assert false (* there is only way to deconstruct ('a, 't const) app *)

(* exponential functor *)
type 'b exponential = EXPONENTIAL
type (_,_) app += Exponential : ('a -> 'b) -> ('a, 'b exponential) app

let get_exponential = function
  | Exponential f -> f
  | _ -> assert false (* there is only way to deconstruct ('a, 't exponential) app *)
