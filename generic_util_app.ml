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

(**************************************************)
(* state monad *)
type 'b state = STATE
type (_,_) app += State : ('b -> 'a * 'b) -> ('a, 'b state) app
let run_state = function
  | State f -> f
  | _ -> assert false
let state =
  { return = (fun x -> State (fun s -> (x, s)))
  ; bind = (fun x f ->
        State (fun s ->
            let (y, s') = run_state x s
            in run_state (f y) s'))
  }
let get = State (fun s -> (s, s))
let put s = State (fun _ -> ((), s))

(**************************************************)
(* reader monad *)
type 'b reader = READER
type (_,_) app += Reader : ('b -> 'a) -> ('a, 'b reader) app
let run_reader = function
  | Reader f -> f
  | _ -> assert false
let reader =
  { return = (fun x -> Reader (fun env -> x))
  ; bind = (fun x f ->
        Reader (fun env ->
            let y = run_reader x env
            in run_reader (f y) env))
  }
let ask = Reader (fun x -> x)
let local modify r = Reader (fun env -> run_reader r (modify env))

(**************************************************)
(* IO monad.
   The values are computations that may carry effects.
   The monadic bind is used to sequence the effects. *)
type io = IO_
type (_,_) app += IO : (unit -> 'a) -> ('a, io) app
let run_io = function
  | IO f -> f ()
  | _ -> assert false
let io =
  { return = (fun x -> IO (fun _ -> x))
  ; bind = (fun x f -> IO (fun _ ->
        let y = run_io x
        in run_io (f y)))
  }
let lift_io x = IO x

let inc r =
  let get_r = IO (fun _ -> !r)
  and set_r x = IO (fun _ -> r := x)
  and (>>=) = io.bind
  and return = io.return
  in get_r >>= fun x ->
     set_r (x+1) >>= fun y ->
     return y

(**************************************************)
(* monoids *)
let int_sum =
  { mempty = 0
  ; mappend = ( + )
  }
let int_prod =
  { mempty = 1
  ; mappend = ( * )
  }
let float_sum =
  { mempty = 0.0
  ; mappend = ( +. )
  }
let float_prod =
  { mempty = 1.0
  ; mappend = ( *. )
  }
