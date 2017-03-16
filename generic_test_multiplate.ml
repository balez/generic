[@@@reify_all]
open Generic
open Ty.T
open Ty.Dyn
open Util
open Monad.T

[%%import Monad (liftM, sequenceM)]

(** Examples from the paper "Scrap Your Boilerplate: A Practical Design Pattern for Generic Programming"
    By Ralf Lammel and Simon Peyton Jones *)
module SYB_Examples = struct
  open Gfun.Multiplate

  (* organisation of a company *)

  type name     = string
  type address  = string
  type person   = P of name * address
  type salary   = S of float
  type employee = E of person * salary
  type manager  = employee
  type subunit  = PU of employee | DU of dept
  and  dept     = D of name * manager * subunit list
  type company  = C of dept list

  (* Increasing the salary of everyone in the company by a
     given factor *)

  let increase factor =
    let increase_plate : type a . a ty -> a -> a
      = fun t x -> match t , x with
        | Salary , S amount -> S (factor *. amount)
        | _ -> x
    in map_family {id_plate = increase_plate} Company

  (* Flatten out a named department by taking all of its sub-units
     and making them part of its parent department. The unit
     manager becomes a plain working unit (PU) of its parent's department.
  *)

  let flatten name =
    let unwrap = function
      | DU (D (n, m, us)) when n = name
        -> PU m :: us
      | u -> [u]
    in
    let flatten_plate : type a . a ty -> a -> a
      = fun t x -> match t , x with
        | Dept , D (n, m, us)
          -> D (n, m, Listx.concat_map unwrap us)
        | _ -> x
    in
    map_family {id_plate = flatten_plate} Company

  (* Querying the salary of an employee *)
  let salary name x =
    let goal = function
        | Dyn (Employee, E (P (n,_), S x))
          when n = name -> Some x
        | _ -> None
    in Listx.find_some goal (family Company x)

  (* summing up all the salaries *)
  let salary_bill =
    let bill_plate = function
      | Dyn (Salary, S x) -> x
      | _ -> 0.0
    in
    pre_fold Monoid.float_sum bill_plate Company

end

module Compos_Examples = struct
  open Gfun.Uniplate
  open Monad.State
  open Monad.Reader

  type expr =
    | Cst of int
    | Neg of expr
    | Add of expr * expr
    | Sub of expr * expr
    | Var of string
    | Let of string * expr * expr

  let rec constants = function
    | Cst x -> [x]
    | Neg x -> constants x
    | Add (x, y) -> constants x @ constants y
    | Sub (x, y) -> constants x @ constants y
    | Var n -> []
    | Let (n, x, y) -> constants x @ constants y

  let is_cst = function
    | Cst k -> [k]
    | _ -> []

  let constants' e =
    Listx.concat_map is_cst (family Expr e)

  let height a = para a @@ fun _ -> function
    | [] -> 0
    | h :: hs -> 1 + List.fold_left max h hs

  module Env = Map.Make (struct type t = string;; let compare = Pervasives.compare end)
  type env = expr Env.t [@@dont_reify]

  let rec subst : env -> expr -> expr
    = fun env -> let open Env in function
        | Let (n, x, y) -> let env' = filter (fun n' _ -> n <> n') env
          in Let (n, subst env x, subst env' y)
        | Var n when mem n env -> find n env
        | x -> map_children Expr (subst env) x

  let simplify = map_family Expr @@ function
    | Neg (Neg x) -> x
    | x -> x

  let const_fold = map_family Expr @@ function
    | Add (Cst x, Cst y) -> Cst (x + y)
    | Sub (Cst x, Cst y) -> Cst (x - y)
    | Neg (Cst x) -> Cst (-x)
    | x -> x

  let simplify_more = reduce_family Expr @@ function
    | Neg (Neg x) -> Some x
    | Sub (x, y) -> Some (Add (x, Neg y))
    | _ -> None

  let incr = let (>>=) = state.bind and return = state.return in
    get         >>= fun i ->
    put (i+1)   >>= fun () ->
    return i

  let abstract_state = traverse_family state Expr @@ function
    | Cst _ -> liftM state (fun i -> Var ("x" ^ string_of_int i)) incr
    | x -> state.return x

  let abstract e = run_state (abstract_state e) 0

  module Free_Vars = struct
    type scoped = string list reader [@@dont_reify]
      let in_scope n = Reader (List.mem n)
      let extend_scope n = local (fun ns -> n :: ns)
      let free_vars_scoped = para Expr @@ fun expr rs ->
        let r = liftM reader List.concat (sequenceM reader rs)
        in match expr with
        | Var n -> reader.bind (in_scope n) (fun is_in_scope ->
            reader.return (if is_in_scope then [] else [n]))
        | Let (n, _, _) -> extend_scope n r
        | _ -> r
      let free_vars x = run_reader (free_vars_scoped x) []
  end
end
