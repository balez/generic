(** Uniplate view

    The uniplate view represents recursive types as rose trees.

    It is inspired by the Haskell uniplate by Neil Mitchell.

- http://hackage.haskell.org/package/uniplate
- http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm

*)

open Generic_core
open Generic_util
open Generic_view

open Ty.T
open Desc.T
open App.T

let local_invalid_arg str = invalid_arg (__MODULE__ ^ str)

let desc = Desc_fun.view
let equal = Equal.equal

let rec scrap_spine : type a b . a ty -> b Spine.t -> a list * (a list -> b Spine.t)
  = fun a s -> let open Spine.T in match s with
    | Con c -> ([], Fun.const s)
    | App (f, c, x) ->
      let (cs, replace) = scrap_spine a f
      in match equal a c with
      | Some Equal.Refl ->
        ( x :: cs
        , function (y :: cs) -> App (replace cs, c, y)
                 | _ -> local_invalid_arg "scrap_spine, replacing an incorrect number of children.")
      | None -> (cs, fun cs -> App (replace cs, c, x))

let scrap a x =
  let (cs, rep) = scrap_spine a (Spine.view a x)
  in (List.rev cs, fun cs -> Spine.rebuild (rep (List.rev cs)))

let children a x = fst (scrap a x)
let replace_children a x = snd (scrap a x)

let rec family a x = x :: List.concat (List.map (family a) (children a x))

let map_children a f x =
  let (cs, rep) = scrap a x
  in rep (List.map f cs)

let rec map_family a f x = f (map_children a (map_family a f) x)

let rec reduce_family a f x =
  let rec g x = match f x with None -> x | Some y -> map_family a g y
  in map_family a g x

let rec para a f x =
  f x (List.map (para a f) (children a x))

(* Applicative and monadic variants *)

let traverse_children t a f x =
  let (cs, rep) = scrap a x
  in (App.fun_of_app t).fmap rep (Listx.traverse t f cs)

let rec traverse_family m a f x = let open App in
  join m (liftM m f (traverse_children (app_of_mon m) a (traverse_family m a f) x))

let rec mreduce_family m a f x =
  let rec g x = m.bind (f x) (function None -> m.return x
                                     | Some y -> traverse_family m a g y)
  in traverse_family m a g x
