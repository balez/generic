open Generic_core
open Generic_util
open Generic_view
open Ty.T
open Desc

type 'a scrapped =
  Scrapped : 'b Product.t * 'b * ('b -> 'a) -> 'a scrapped

let scrap_conlist : 'a Conlist.t -> 'a -> 'a scrapped
  = fun cs x -> match Conlist.conap cs x with
    | Con.Conap (c, y) -> Scrapped (Con.product c, y, c.embed)

let scrap : 'a ty -> 'a -> 'a scrapped
  = fun t x -> match Conlist.view t with
    | Some cs -> scrap_conlist cs x
    | None -> Scrapped (Product.Nil, (), Fun.const x)
