open Generic_core
open Ty.T

type 'a scrapped =
  Scrapped : 'b Product.t * 'b * ('b -> 'a) -> 'a scrapped

val scrap : 'a ty -> 'a -> 'a scrapped
