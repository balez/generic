(** Uniplate view

    The uniplate view represents recursive types as rose trees.

    It is inspired by the Haskell uniplate and multiplate libraries.

http://hackage.haskell.org/package/uniplate
http://community.haskell.org/~ndm/darcs/uniplate/uniplate.htm

*)

open Generic_core
open Generic_util
open Generic_view

open Ty.T
open Desc.T

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

let scrap a b x =
  let (cs, rep) = scrap_spine b (Spine.view a x)
  in (List.rev cs, fun cs -> Spine.rebuild (rep (List.rev cs)))

let children a b x = fst (scrap a b x)
let replace_children a b x = snd (scrap a b x)

let map_children a b f x =
  let (cs, rep) = scrap a b x
  in rep (List.map f cs)

let rec family a x = x :: child_families a a x
and child_families : 'a . 'a ty -> 'b ty -> 'a -> 'b list
  = fun a b x -> List.concat (List.map (family b) (children a b x))

let rec map_family a f x = f (map_child_families a a f x)
and map_child_families : 'a . 'a ty -> 'b ty -> ('b -> 'b) -> ('a -> 'a)
  = fun a b f x -> map_children a b (map_family b f) x

let rec recurse_family a f x =
  let rec g x = Option.unopt x (map_family a g) (f x)
  in map_family a g x

let recurse_child_families a b f x =
  map_children a b (recurse_family b f) x

let rec fold a f x = fold_children a a f f x
and fold_children : 'a 'ra . 'a ty -> 'b ty -> ('a -> 'rb list -> 'ra) -> ('b -> 'rb list -> 'rb) -> 'a -> 'ra
  = fun a b f g x ->
  f x (List.map (fold b g) (children a b x))
