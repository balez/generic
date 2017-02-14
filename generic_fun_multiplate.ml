open Generic_core
open Ty.T

type 'a scrapped =
  Scrapped : 'b Product.t * 'b * ('b -> 'a) -> 'a scrapped

let scrap : 'a ty -> 'a -> 'a scrapped
  = fun x -> invalid_arg ""
