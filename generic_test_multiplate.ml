[@@@reify_all]
open Generic
open Fun.Multiplate
open Ty.T
open Ty.Dyn
open Util

(** Examples from the paper "Scrap Your Boilerplate: A Practical Design Pattern for Generic Programming"
    By Ralf Lammel and Simon Peyton Jones *)
module SYB_Examples = struct
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
          -> D (n, m, Listx.concatmap unwrap us)
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
    pre_fold App.float_sum bill_plate Company


end
