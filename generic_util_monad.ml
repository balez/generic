(** Monads and basic operations. *)

open Generic_util

module type S = sig
  type (+'a) t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Operations (M : S) = struct
  include M
  let map f m = bind m (fun x -> return (f x))
  let join mm = bind mm (fun m -> m)

  let bind2 xm ym f =
    bind xm (fun x ->
        bind ym (fun y ->
            f x y))
  let bind3 xm ym zm f =
    bind2 xm ym (fun x y ->
        bind zm (fun z ->
            f x y z))

  let bind4 m1 m2 m3 m4 f =
    bind3 m1 m2 m3 (fun x1 x2 x3 ->
        bind m4 (fun x4 ->
            f x1 x2 x3 x4))

  let map2 f x y = bind2 x y (fun x y -> return (f x y))
  let map3 f x y z = bind3 x y z (fun x y z -> return (f x y z))
  let map4 f x y z a = bind4 x y z a (fun x y z a -> return (f x y z a))

  let rec for_each f = function
    | [] -> return []
    | x :: xs ->
      map2 Listx.cons (f x) (for_each f xs)

  let rec sequence ms = for_each Fun.id ms

  let erase m = map (Fun.const ()) m
end
