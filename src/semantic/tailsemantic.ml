open Symbols_table
open Scopechecker
open Typechecker
open Ast

let check_semantics ast =
  let st = new symbols_table None in
  let st = fill_predefined st in
  let st = fill_constants ast st in
  st#reset_child_position;
  match scope_check ast st with
  | Ok st -> begin match st#reset_child_position; type_check ast st with
             | Ok (_, ast) -> Ok ast
             | Error (i, msg) -> Error (i, msg)
             end
  | Error (i, msg) -> Error (i, msg)


(* Prints pretty error messages *)
let print_semantic_error ic info msg =
  let error_color = "\027[38;2;231;29;54m" in
  let position_color = "\027[38;2;0;159;183m" in
  let bold = "\027[1m" in
  let reset = "\027[0m\027[38;2;40;40;40m" in
  let (start_index, start_line_num, start_column_num) = info.start_pos in
  let (end_index, end_line_num, end_column_num) = info.end_pos in
  let line = seek_in ic (start_index - start_column_num + 1); input_line ic in
  let rec trim_index str i =
    if str.[i] = '\t' || str.[i] = ' ' || str.[i] = '\012' then
      trim_index str (i+1)
    else i
  in
  let t_index = trim_index line 0 in
  let line = String.trim line in
  Printf.fprintf stderr
    "\n%s%sSemantic error:%s\n"
    bold error_color reset;
  Printf.fprintf stderr
    "%sIn %sline %d%s%s, %scolumn %d%s\n"
    bold
    position_color start_line_num reset bold
    position_color start_column_num reset;
  Printf.fprintf stderr "\n\t%s\n" line;
  Printf.fprintf stderr
    "\t%s%s%s^%s%s\n\n"
    (String.make (start_column_num - 1 - t_index) ' ')
    bold error_color (String.make (end_column_num - start_column_num - 1) '~')
    reset;
  Printf.fprintf stderr
    "%s%s%s\n\n"
    bold msg reset;
  Printf.fprintf stderr "\027[0m";
