open Symbols_table
open Scopechecker

let check_semantics ast =
  let st = new symbols_table None in
  (fill_constants ast st)#to_string "" |> print_endline
