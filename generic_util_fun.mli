(** Function combinators. *)

(** Constant function, ignore the second argument. *)
val const : 'a -> 'b -> 'a

(** Identity function *)
val id : 'a -> 'a

(** flip the arguments of a function *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(** {2 Function composition}

    The functions [res_n] compose a unary function to
    the result of a [n]-ary function, so that:
{[
res_n g f x_0 .. x_{n-1} == g (f x_0 ... x_{n-1})
]}
*)

val res1 : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val res2 : ('a -> 'b) -> ('c -> 'd -> 'a) -> 'c -> 'd -> 'b

val res3 : ('a -> 'b) -> ('c -> 'd -> 'e -> 'a) -> 'c -> 'd -> 'e -> 'b
val res4 : ('a -> 'b) -> ('c -> 'd -> 'e -> 'f -> 'a) -> 'c -> 'd -> 'e -> 'f -> 'b
val res5 :
  ('a -> 'b) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'b)
val res6 :
  ('a -> 'b) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'a) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'b)
val res7 :
  ('a -> 'b) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'a) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'b)
val res8 :
  ('a -> 'b) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'a) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'b)
val res9 :
  ('a -> 'b) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'a) ->
  ('c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'b)


(** [opres (^) f g x = f x ^ g x] *)
val opres : ('a -> 'b -> 'c) -> ('d -> 'a) -> ('d -> 'b) -> 'd -> 'c

(** [opon (^) f x y = f x ^ f y] *)
val opon : ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b

(** {3 Composition and Point-Free Operators} *)

(** [(g -< f) x = g (f x)] *)
val ( -< ) : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)

(** [(f >- g) x = g (f x)] *)
val ( >- ) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)

(** [(f >>. g) x = f x ; g x] *)
val ( >>. ) : ('a -> 'b) -> ('a -> 'c) -> ('a -> 'c)

(** [(f &&. g) x = f x && g x] *)
val ( &&. ) : ('a -> bool) -> ('a -> bool) -> ('a -> bool)

(** {2 Isomorphisms}
*)
(** An isomorphism is given by a bijection and its inverse,
it must be true that: {[fwd (bck x) = bck (fwd x) = x]}
*)
type ('a, 'b) iso = { fwd : 'a -> 'b; bck : 'b -> 'a; }

(** The identity isomorphism *)
val iso_id : ('a, 'a) iso

(** {2 Functions on ordered types}
*)

(** {[in_range x (a, b)  =  x >= a && x <= b]} *)
val in_range : 'a -> 'a * 'a -> bool
