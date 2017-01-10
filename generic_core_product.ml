open Generic_core
open Generic_util

(** This module defines the [product] datatype and is meant
    to be open so that constructors can be used in pattern
    matching. *)
module T = struct
  type 'a product =
  | Nil : unit product
  | Cons : 'a Ty.ty * 'b product -> ('a * 'b) product
end
include T

(** Synonym, so that [Product.T.product] might be refered to as [Product.t] *)
type 'a t = 'a product

module Build = struct
  let pc x y = Cons (x,y)
  let p0 = Nil
  let p1 x = pc x p0
  let p2 x = Fun.res1 (pc x) p1
  let p3 x = Fun.res2 (pc x) p2
  let p4 x = Fun.res3 (pc x) p3
  let p5 x = Fun.res4 (pc x) p4
  let p6 x = Fun.res5 (pc x) p5
  let p7 x = Fun.res6 (pc x) p6
  let p8 x = Fun.res7 (pc x) p7
  let p9 x = Fun.res8 (pc x) p8
  let p10 x = Fun.res9 (pc x) p9
end

include Build

let uncurry1 f (x,()) = f x
let uncurry2 f = Fun.res1 uncurry1 (Fun.uncurry f)

let rec fold : type a . (Ty.ty' -> 'b -> 'b) -> 'b -> a t -> 'b
  = fun c n ->
  function
  | Nil -> n
  | Cons (t,p) -> c (Ty.E t) (fold c n p)

let length p =
  fold (fun _ n -> n + 1) 0 p

let list_of_prod p =
  fold (fun h t -> h :: t) [] p

(* * Dynamic Products *)
type 'a tuple = 'a t * 'a
type dynprod = Dynprod : 'a tuple -> dynprod

let rec list_of_dynprod = function
  | Dynprod (Nil,()) -> []
  | Dynprod (Cons (t, ts), (x,xs)) ->
     Ty.Dyn (t,x) :: list_of_dynprod (Dynprod (ts, xs))

let rec dynprod_of_list = function
  | [] -> Dynprod (Nil,())
  | Ty.Dyn (t,x) :: ds ->
    let Dynprod (ts, xs) = dynprod_of_list ds in
    Dynprod (Cons (t, ts), (x,xs))
