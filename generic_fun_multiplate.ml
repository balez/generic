open Generic_core
open Generic_util
open Generic_view
open Ty.T
open Ty.Dyn
open Desc
open App.T

type 'f plate = {plate : 'a . 'a ty -> 'a -> ('a,'f) App.t}
type id_plate = {id_plate : 'a . 'a ty -> 'a -> 'a}
type 'b const_plate = {const_plate : 'a . 'a ty -> 'a -> 'b}
type 'b dyn_plate = Ty.dyn -> 'b

let const_of_dyn_plate p =
  { const_plate = fun t x -> p (Dyn (t,x)) }
let dyn_of_const_plate p (Dyn (t,x)) = p.const_plate t x

let id_plate {id_plate} =
  {plate = fun t x -> App.Id (id_plate t x)}

let const_plate {const_plate} =
  {plate = fun t x -> App.Const (const_plate t x)}

let compose_monad {return;bind} g f =
  {plate = fun t x -> bind (f.plate t x) (fun y -> g.plate t y)}

let compose_right_id g f =
  {plate = fun t x -> g.plate t (f.id_plate t x)}

let compose_left_id {fmap} g f =
  {plate = fun t x -> fmap (g.id_plate t) (f.plate t x)}

let compose {fmap} g f =
  {plate = fun t x -> App.Comp (fmap (g.plate t) (f.plate t x))}

let compose_id g f =
  {id_plate = fun t x -> g.id_plate t (f.id_plate t x)}

let append_plate m g f =
  {const_plate = fun t x -> m.mappend (g.const_plate t x) (f.const_plate t x)}

(****************************************************)
let pure_plate {pure; apply} =
  { plate = fun t x -> pure x }
let pure_id_plate =
  { id_plate = fun t x -> x }
let pure_const_plate {mempty; _} =
  { const_plate = fun t x -> mempty }

(****************************************************)

let traverse a {plate} p x
  = let rec go : type p . p Product.t * p -> (p, 'f) App.t
    = let open Product in function
      | Nil , () -> a.pure ()
      | Cons (t, ts) , (x, xs) ->
        let pair a b = (a,b) in
        App.liftA2 a pair (plate t x) (go (ts, xs))
    in go (p,x)

let map f p x = App.get_id (traverse App.id_applicative (id_plate f) p x)

(****************************************************)
type 'a scrapped =
  Scrapped : 'b Product.t * 'b * ('b -> 'a) -> 'a scrapped

let scrap_conlist : 'a Conlist.t -> 'a -> 'a scrapped
  = fun cs x -> match Conlist.conap cs x with
    | Con.Conap (c, y) -> Scrapped (Con.product c, y, c.embed)

let scrap : 'a ty -> 'a -> 'a scrapped
  = fun t x -> match Conlist.view t with
    | [] -> Scrapped (Product.Nil, (), Fun.const x)
    | cs -> scrap_conlist cs x

let children t x =
  let Scrapped (p, cs, rep) = scrap t x
  in let open Product in
  list_of_dynprod (Dynprod (p, cs))
let children_d (Dyn (t,x)) = children t x

let traverse_children_p a f = {plate = fun t x ->
  let Scrapped (p, cs, rep) = scrap t x
  in (App.fun_of_app a).fmap rep (traverse a f p cs)}
let traverse_children a f = (traverse_children_p a f).plate

let map_children_p f = {id_plate = fun t x -> let open App in
  get_id (traverse_children id_applicative (id_plate f) t x)}
let map_children f = (map_children_p f).id_plate

let rec traverse_family_p m f =
  compose_monad m f (traverse_children_p (App.app_of_mon m) (traverse_family_p m f))
let traverse_family m f = (traverse_family_p m f).plate

let rec map_family_p f = {id_plate = fun t x -> let open App in
  get_id (traverse_family id_monad (id_plate f) t x)}
let map_family f = (map_family_p f).id_plate

let fold_children_p m f = {const_plate = fun t x ->
  let open App in
  get_const (traverse_children (const_applicative m) (const_plate f) t x)}
let fold_children_d m f =
  dyn_of_const_plate (fold_children_p m (const_of_dyn_plate f))
let fold_children m f =
  (fold_children_p m (const_of_dyn_plate f)).const_plate

let rec pre_fold_p m f =
  append_plate m f (fold_children_p m (pre_fold_p m f))
let pre_fold_d m f =
  dyn_of_const_plate (pre_fold_p m (const_of_dyn_plate f))
let pre_fold m f =
  (pre_fold_p m (const_of_dyn_plate f)).const_plate

let rec post_fold_p m f =
  append_plate m (fold_children_p m (post_fold_p m f)) f
let post_fold_d m f =
  dyn_of_const_plate (post_fold_p m (const_of_dyn_plate f))
let post_fold m f =
  (post_fold_p m (const_of_dyn_plate f)).const_plate

let family t x =
  pre_fold Listx.monoid (fun x -> [x]) t x
let family_d (Dyn (t,x)) =
  family t x

let rec para_d step x =
  step x (List.map (para_d step) (children_d x))
let para step t x = para_d step (Dyn (t,x))
let para_p step =
  const_of_dyn_plate (para_d (dyn_of_const_plate step))
