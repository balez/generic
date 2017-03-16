(** Applicative functors. *)

open Generic_util
open App.T
open App
open Monoid.T

module T = struct
  type 'f applicative =
    { pure : 'a . 'a -> ('a,'f) app
    ; apply : 'a 'b . ('a -> 'b, 'f) app -> ('a, 'f) app -> ('b, 'f) app
    }
end
include T
type 'f t = 'f applicative

(** {2 Operations} *)

let fun_of_app {pure; apply} =
  {Functor.fmap = fun f -> apply (pure f)}

let liftA a f x = (fun_of_app a).fmap f x
let liftA2 a f x y = a.apply (liftA a f x) y
let liftA3 a f x y z = a.apply (liftA2 a f x y) z
let liftA4 a f u v w x = a.apply (liftA3 a f u v w) x

(** {3 Traversing lists of effectful elements } *)

let rec traverse a f =
  let cons h t = h :: t in function
    | [] -> a.pure []
    | h :: t -> liftA2 a cons (f h) (traverse a f t)

let sequence a = traverse a (fun x -> x)

(** {2 Instances} *)

let id =
  { pure = (fun x -> Id x)
  ; apply = (fun f x -> Id ((get_id f) (get_id x)))
  }

let const {mempty;mappend} =
  { pure = (fun _ -> Const mempty)
  ; apply = (fun f x -> Const (mappend (get_const f) (get_const x)))
  }

let option =
  let option_apply f x = match f , x with
    | Some f , Some x -> Some (f x)
    | _  -> None
  in
  { pure = (fun x -> Option (Some x))
  ; apply = (fun fs xs -> Option (option_apply (get_option fs) (get_option xs)))
  }

let list =
  let list_apply fs xs =
    List.concat (List.map (fun f -> List.map f xs) fs)
  in
  { pure = (fun x -> List [x])
  ; apply = (fun fs xs -> List (list_apply (get_list fs) (get_list xs)))
  }
