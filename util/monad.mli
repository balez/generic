(** Monads. *)

open Generic_util
open App.T
open Functor.T
open Applicative.T

module T :
  sig
    type 'f monad = {
      return : 'a. 'a -> ('a, 'f) app;
      bind :
        'a 'b.
          ('a, 'f) app ->
          ('a -> ('b, 'f) app) ->
          ('b, 'f) app;
    }
  end
type 'f monad =
  'f T.monad = {
  return : 'a. 'a -> ('a, 'f) app;
  bind :
    'a 'b.
      ('a, 'f) app ->
      ('a -> ('b, 'f) app) ->
      ('b, 'f) app;
}
type 'f t = 'f monad

(** {2 Operations} *)

val fun_of_mon : 'a monad -> 'a functorial
val app_of_mon : 'a monad -> 'a applicative
val join :
  'a monad ->
  (('b, 'a) app, 'a) app ->
  ('b, 'a) app
val liftM :
  'a monad ->
  ('b -> 'c) ->
  ('b, 'a) app -> ('c, 'a) app
val liftM2 :
  'a monad ->
  ('b -> 'c -> 'd) ->
  ('b, 'a) app ->
  ('c, 'a) app -> ('d, 'a) app
val liftM3 :
  'a monad ->
  ('b -> 'c -> 'd -> 'e) ->
  ('b, 'a) app ->
  ('c, 'a) app ->
  ('d, 'a) app -> ('e, 'a) app
val liftM4 :
  'a monad ->
  ('b -> 'c -> 'd -> 'e -> 'f) ->
  ('b, 'a) app ->
  ('c, 'a) app ->
  ('d, 'a) app ->
  ('e, 'a) app -> ('f, 'a) app

(** {3 Traversing lists of effectful elements} *)

val traverseM : 'f monad -> ('a -> ('b, 'f) app) -> 'a list -> ('b list, 'f) app
val sequenceM : 'f monad -> ('a, 'f) app list -> ('a list, 'f) app

(** {2 Instances} *)

val id : App.id monad
val option : App.option' monad
val list : App.list' monad

(** {3 State Monad} *)

module State : sig
  type 'b state = STATE
  type (_, _) app +=
      State : ('b -> 'a * 'b) -> ('a, 'b state) app
  val run_state : ('a, 'b state) app -> 'b -> 'a * 'b
  val state : 'a state monad
  val get : ('a, 'a state) app
  val put : 'a -> (unit, 'a state) app
end

(** {3 Reader Monad} *)

module Reader : sig
  type 'b reader = READER
  type (_, _) app +=
      Reader : ('b -> 'a) -> ('a, 'b reader) app
  val run_reader : ('a, 'b reader) app -> 'b -> 'a
  val reader : 'a reader monad
  val ask : ('a, 'a reader) app
  val local :
    ('a -> 'b) ->
    ('c, 'b reader) app ->
    ('c, 'a reader) app
end

(** {3 IO Monad} *)

module IO : sig

(** Similar to the Haskell IO Monad.
The values are computations that may carry effects.
The monadic bind is used to sequence the effects.

The constructor {!IO} and synonym {!embed_io} allow to embed an effectful computation in the monad.
{!run_io} runs it.
 *)

(** {!io} is used as a type index for {!T.app} *)
type io = IO_

(** Embed an effectful computation in the IO monad *)
type (_, _) app += IO : (unit -> 'a) -> ('a, io) app

val embed_io : (unit -> 'a) -> ('a, io) app
(** Embed an effectful computation in the IO monad *)

val run_io : ('a, io) app -> 'a
(** Run the computation with side effects to obtain a result *)

val io : io monad
(** The IO monad operations *)

end
