let const x _ = x
let id x = x
let flip f x y = f y x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

(* composition operators.
   "resN" stands for:
      apply a function on the result of an "n"-ary function *)
let res1 f g x = f (g x)
(* res2 f g x y = f (g x y) *)
(* res1 res1 res1 f g x y == res1 (res1 f) g x y == res1 f (g x) y == f (g x y) *)
let res2 f = res1 res1 res1 f
(* res1 res1 res2 f g x y z == res1 (res2 f) g x y z == res2 f (g x) y z == f (g x y z) *)
let res3 f = res1 res1 res2 f
let res4 f = res1 res1 res3 f
let res5 f = res1 res1 res4 f
let res6 f = res1 res1 res5 f
let res7 f = res1 res1 res6 f
let res8 f = res1 res1 res7 f
let res9 f = res1 res1 res8 f
let res10 f = res1 res1 res9 f

let opres op f g = fun x -> op (f x) (g x)
let opon op f x y = op (f x) (f y)

(* Composition and point-free operators *)
let (-<) f g x = f (g x)
let (>-) f g x = g (f x)
let (>>.) f g x = (f x ; g x)
let (&&.) f g x = f x && g x

(* forward and backward arrows, fwd (bck x) = bck (fwd x) = x *)
type ('a,'b) iso = {fwd : 'a -> 'b; bck : 'b -> 'a}
let iso_id = {fwd = id; bck= id}

let in_range x (a, b) = x >= a && x <= b
