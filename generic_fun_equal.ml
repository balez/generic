open Generic_core
open Generic_util
open Generic_view

module Sumprod = struct
  let rec equal : type a . a Ty.t -> a -> a -> bool
    = fun t -> equal_sp (Sumprod.view t)

  and equal_sp : type a . a Sumprod.sp -> a -> a -> bool
    = fun s x y ->
      let open Sumprod in match s with
      | Unit -> true
      | Prod (a, b) -> (match x , y with
          | (xa, xb) , (ya, yb)
            -> equal_sp a xa ya && equal_sp b xb yb)
      | Sum (a, b) -> (match x , y with
          | Left xa  , Left ya  -> equal_sp a xa ya
          | Right xb , Right yb -> equal_sp b xb yb
          | _ , _ -> false)
      | Delay t -> equal t x y
      | Con (_, a) -> equal_sp a x y
      | Field (_, a) -> equal_sp a x y
      | Iso (s, fb) ->
        equal_sp s (fb.bck x) (fb.bck y)
      | Base t -> x = y
      | Empty -> Sum.empty_elim x
end

module Conlist = struct
  open Conlist

  let rec equal : type a . a Ty.t -> a -> a -> bool
    = fun t -> match view t with
      | None -> ( = )              (* Base case (core types) *)
      | Some cs -> equal_conlist cs (* generic case *)

  and equal_conlist : type a . a Conlist.view -> a -> a -> bool
    = fun cs x y -> match conap cs x with
      | Desc.Con.Conap (c,x') -> match c.proj y with
        | None -> false
        | Some y' -> equal_prod (Desc.Con.product c) x' y'

  and equal_prod : type p . p Product.t -> p -> p -> bool
    = let open Product.T in function
      | Nil -> fun _ _ -> true
      | Cons (t, ts) -> fun (x,xs) (y,ys) -> equal t x y && equal_prod ts xs ys
end
