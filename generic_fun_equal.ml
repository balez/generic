open Generic_core
open Generic_util
open Generic_view

let rec equal_sp : type a . a Sumprod.sp -> a * a -> bool
  = let open Sumprod in function
    | Unit -> Fun.const true
    | Prod (a, b) -> (function
        | (xa, xb), (ya, yb)
        -> equal_sp a (xa, ya) && equal_sp b (xb, yb))
    | Sum (a, b) -> (function
      | Left xa, Left ya -> equal_sp a (xa, ya)
      | Right xb, Right yb -> equal_sp b (xb, yb)
      | _ , _ -> false)
    | Con (_, a) -> equal_sp a
    | Field (_, a) -> equal_sp a
    | Iso (fb, a) -> fun (x, y) ->
        equal_sp a (fb.bck x, fb.bck y)
    | Base t -> fun (x,y) -> x = y
    | Empty -> fun (x,y) -> Sum.empty_elim x

let equal : 'a Ty.t -> 'a -> 'a -> bool
  = fun t x y -> equal_sp (Sumprod.view t) (x,y)
