(* Dealing with parametric types using defunctionalisation. *)
open Generic_util

module T = struct
  type ('a,'b) app = ..
  type 'f functorial =
    { fmap : 'a 'b . ('a -> 'b) -> ('a, 'f) app -> ('b, 'f) app }

  type 'f applicative =
    { pure : 'a . 'a -> ('a,'f) app
    ; apply : 'a 'b . ('a -> 'b, 'f) app -> ('a, 'f) app -> ('b, 'f) app
    }

  type 'f monad =
    { return : 'a . 'a -> ('a,'f) app
    ; bind : 'a 'b . ('a , 'f) app -> ('a -> ('b, 'f) app) -> ('b, 'f) app
    }

  type 't monoid =
    { mempty : 't
    ; mappend : 't -> 't -> 't
    }
end

open T
type ('a,'b) t = ('a,'b) app

(** {2 Core parametric  types} *)

type option' = OPTION
type (_,_) app += Option : 'a option -> ('a, option') app
let get_option = function
  | Option x -> x
  | _ -> assert false

type list' = LIST
type (_,_) app += List : 'a list -> ('a, list') app
let get_list = function
  | List x -> x
  | _ -> assert false

type array' = ARRAY
type (_,_) app += Array : 'a array -> ('a, array') app
let get_array = function
  | Array x -> x
  | _ -> assert false


(* Identity functor *)
type id = ID
type (_, _) app += Id : 'a -> ('a, id) app
let get_id = function
  | Id x -> x
  | _ -> assert false (* there is only way to deconstruct ('a, 't id) app *)

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

(* functor composition *)

type ('f, 'g) comp = COMP
type (_, _) app += Comp : (('a,'f) app, 'g) app -> ('a, ('f, 'g) comp) app
let get_comp = function
  | Comp x -> x
  | _ -> assert false


(***************************************************)
let fun_of_app {pure; apply} =
  {fmap = fun f -> apply (pure f)}
let fun_of_mon {return; bind} =
  {fmap = fun f kx -> bind kx (fun x -> return (f x))}
let app_of_mon ({return; bind} as m) =
  { pure = return
  ; apply = fun kf kx -> bind kf (fun f -> (fun_of_mon m).fmap f kx)
  }

let liftA a f x = (fun_of_app a).fmap f x
let liftA2 a f x y = a.apply (liftA a f x) y
let liftA3 a f x y z = a.apply (liftA2 a f x y) z
let liftA4 a f u v w x = a.apply (liftA3 a f u v w) x

let join {return;bind} mm = bind mm Fun.id
let liftM m = liftA (app_of_mon m)
let liftM2 m = liftA2 (app_of_mon m)
let liftM3 m = liftA3 (app_of_mon m)
let liftM4 m = liftA4 (app_of_mon m)

let id_functor =
  { fmap = (fun f x -> Id (f (get_id x))) }

let id_applicative =
  { pure = (fun x -> Id x)
  ; apply = (fun f x -> Id ((get_id f) (get_id x)))
  }

let id_monad =
  { return = (fun x -> Id x)
  ; bind = (fun x f -> f (get_id x))
  }

let const_applicative {mempty;mappend} =
  { pure = (fun _ -> Const mempty)
  ; apply = (fun f x -> Const (mappend (get_const f) (get_const x)))
  }
