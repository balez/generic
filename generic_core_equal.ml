open Generic_core
open Generic_util
open App.T
open Ty.T

let local_invalid_arg s = invalid_arg (__MODULE__ ^ "." ^ s)

(* type equality,
x : ('a,'b) equal is a witness that 'a and 'b are intensionally equal. *)
type (_,_) equal = Refl : ('a,'a) equal
type ('a,'b) t = ('a,'b) equal


type tag = Tag
type (_,_) app += App : {f : 'b . 'b Ty.t -> ('a, 'b) equal option} -> ('a, tag) app

let equal_closure = Extensible.create "Generic_core_equal.equal" (* private *)

let equal a b = match equal_closure.f a with
  | App x -> x.f b
  | _ -> assert false

type equal_fun =
  { f : 'a 'b . 'a Ty.t -> 'b Ty.t -> ('a, 'b) equal option }

let ext t f = equal_closure.ext t { f = fun a -> App {f = fun b -> f.f a b} }

let coerce : type a b . a ty -> b ty -> a -> b option
 = fun a b x ->
  match equal a b with
  | Some Refl -> Some x
  | None -> None

(* equal for core types *)

let () =
  ext Int32 { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Int32, Int32 -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Int64 { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Int64, Int64 -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Nativeint { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Nativeint, Nativeint -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Exn { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Exn, Exn -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Bool { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Bool, Bool -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Int { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Int, Int -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Float { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Float, Float -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Char { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Char, Char -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Bytes { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Bytes, Bytes -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext String { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | String, String -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext Unit { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Unit, Unit -> Some (Refl : (a, b) equal)
      | _ , _ -> None };
  ext (Lazy Any) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Lazy x, Lazy y -> (match equal x y with
        | Some Refl -> Some (Refl : (a, b) equal)
        | None -> None)
      | _ , _ -> None };
  ext (Option Any) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Option x, Option y -> (match equal x y with
        | Some Refl -> Some (Refl : (a, b) equal)
        | None -> None)
      | _ , _ -> None };
  ext (List Any) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | List x, List y -> (match equal x y with
        | Some Refl -> Some (Refl : (a, b) equal)
        | None -> None)
      | _ , _ -> None };
  ext (Array Any) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Array x, Array y -> (match equal x y with
        | Some Refl -> Some (Refl : (a, b) equal)
        | None -> None)
      | _ , _ -> None };
  ext (Ref Any) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Ref x, Ref y -> (match equal x y with
        | Some Refl -> Some (Refl : (a, b) equal)
        | None -> None)
      | _ , _ -> None };
  ext (Ty Any) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Ty x, Ty y -> (match equal x y with
        | Some Refl -> Some (Refl : (a, b) equal)
        | None -> None)
      | _ , _ -> None };
  ext Ty.pair { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Pair (x,y), Pair (x',y') -> (match equal x x', equal y y' with
        | Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _ , _ -> None)
      | _ , _ -> None };
  ext Ty.triple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Triple (x,y,z), Triple (x',y',z') ->
        (match equal x x', equal y y', equal z z' with
        | Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.quadruple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Quadruple (a,b,c,d), Quadruple (a',b',c',d') ->
        (match equal a a', equal b b', equal c c', equal d d' with
        | Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.quintuple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Quintuple (a,b,c,d,e), Quintuple (a',b',c',d',e') ->
        (match equal a a', equal b b', equal c c', equal d d', equal e e' with
        | Some Refl, Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.sextuple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Sextuple (a,b,c,d,e,f), Sextuple (a',b',c',d',e',f') ->
        (match equal a a', equal b b', equal c c', equal d d', equal e e', equal f f' with
        | Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.septuple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Septuple (a,b,c,d,e,f,g), Septuple (a',b',c',d',e',f',g') ->
        (match equal a a', equal b b', equal c c', equal d d', equal e e', equal f f', equal g g' with
        | Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _, _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.octuple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Octuple (a,b,c,d,e,f,g,h), Octuple (a',b',c',d',e',f',g',h') ->
        (match equal a a', equal b b', equal c c', equal d d', equal e e', equal f f', equal g g', equal h h' with
        | Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _, _, _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.nonuple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Nonuple (a,b,c,d,e,f,g,h,i), Nonuple (a',b',c',d',e',f',g',h',i') ->
        (match equal a a', equal b b', equal c c', equal d d', equal e e', equal f f', equal g g', equal h h', equal i i' with
        | Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _, _, _, _, _, _ -> None)
      | _ , _ -> None };
  ext Ty.decuple { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Decuple (a,b,c,d,e,f,g,h,i,j), Decuple (a',b',c',d',e',f',g',h',i',j') ->
        (match equal a a', equal b b', equal c c', equal d d', equal e e', equal f f', equal g g', equal h h', equal i i', equal j j' with
        | Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _, _, _, _, _, _, _, _, _, _ -> None)
      | _ , _ -> None };
  ext (Fun (Any,Any)) { f = fun (type a) (type b) (a : a ty) (b : b ty) ->
      match a , b with
      | Fun (x,y), Fun (x',y') -> (match equal x x', equal y y' with
        | Some Refl, Some Refl -> Some (Refl : (a, b) equal)
        | _ , _ -> None)
      | _ , _ -> None };
