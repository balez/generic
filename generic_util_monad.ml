(** Monads. *)

open Generic_util
open App.T
open App

let ( -< ) = Fun.( -< )

module T = struct
  type 'f monad =
    { return : 'a . 'a -> ('a,'f) app
    ; bind : 'a 'b . ('a , 'f) app -> ('a -> ('b, 'f) app) -> ('b, 'f) app
    }
end
include T
type 'f t = 'f monad

(** {2 Operations} *)

let fun_of_mon {return; bind} =
  { Functor.fmap = fun f kx -> bind kx (fun x -> return (f x)) }

let app_of_mon ({return; bind} as m) =
  { Applicative.pure = return
  ; apply = fun kf kx -> bind kf (fun f -> (fun_of_mon m).fmap f kx)
  }

let join {return;bind} mm = bind mm Fun.id
let liftM m  = Applicative.liftA (app_of_mon m)
let liftM2 m = Applicative.liftA2 (app_of_mon m)
let liftM3 m = Applicative.liftA3 (app_of_mon m)
let liftM4 m = Applicative.liftA4 (app_of_mon m)

(** {2 Instances} *)

let id =
  { return = (fun x -> Id x)
  ; bind = (fun x f -> f (get_id x))
  }

let option =
  { return = (fun x -> Option (Some x))
  ; bind = (fun xs f -> match get_option xs with
        | None -> Option None
        | Some x -> f x)}

let list =
  { return = (fun x -> List [x])
  ; bind = (fun xs f -> List (List.concat (List.map (get_list -< f) (get_list xs))))
  }

(** {3 State Monad} *)

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

(** {3 Reader Monad} *)

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

(** {3 IO Monad} *)

(** The values are computations that may carry effects.
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
let embed_io x = IO x

(** Example *)
let inc r =
  let get_r = IO (fun _ -> !r)
  and set_r x = IO (fun _ -> r := x)
  and (>>=) = io.bind
  and return = io.return
  in get_r >>= fun x ->
     set_r (x+1) >>= fun y ->
     return y
