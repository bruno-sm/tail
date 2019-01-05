include Syntax


let string_of_token = function
  | INT_NUMBER n -> Printf.sprintf "INT_NUMBER(%d)" n
  | REAL_NUMBER x -> Printf.sprintf "REAL_NUMBER(%f)" x
  | NAME n -> Printf.sprintf "NAME(%s)" n
  | ATOM a -> Printf.sprintf "ATOM(%s)" a
  | SEQUENCE -> Printf.sprintf "SEQUENCE"
  | OPEN_PARENTHESES -> Printf.sprintf "OPEN_PARENTHESES"
  | CLOSE_PARENTHESES -> Printf.sprintf "CLOSE_PARENTHESES"
  | LAMBDA -> Printf.sprintf "LAMBDA"
  | COLON -> Printf.sprintf "COLON"
  | BLOCK_BEGIN -> Printf.sprintf "BLOCK_BEGIN"
  | BLOCK_END -> Printf.sprintf "BLOCK_END"
  | ASSIGN -> Printf.sprintf "ASSIGN"
  | COMMA -> Printf.sprintf "COMMA"
  | IF -> Printf.sprintf "IF"
  | THEN -> Printf.sprintf "THEN"
  | ELIF -> Printf.sprintf "ELIF"
  | ELSE -> Printf.sprintf "ELSE"
  | UNION -> Printf.sprintf "UNION"
  | INTERSECTION -> Printf.sprintf "INTERSECTION"
  | COMPLEMENT -> Printf.sprintf "COMPLEMENT"
  | INT_TYPE -> Printf.sprintf "INT_TYPE"
  | REAL_TYPE -> Printf.sprintf "REAL_TYPE"
  | STRING_TYPE -> Printf.sprintf "STRING_TYPE"
  | ATOM_TYPE -> Printf.sprintf "ATOM_TYPE"
  | SPECIFIC_ATOM_TYPE a -> Printf.sprintf "SPECIFIC_ATOM_TYPE(%s)" a
  | LIST_TYPE -> Printf.sprintf "LIST_TYPE"
  | MATRIX_TYPE -> Printf.sprintf "MATRIX_TYPE"
  | UNIVERSE_TYPE n -> Printf.sprintf "UNIVERSE_TYPE(%d)" n
  | UNKNOWN_TYPE -> Printf.sprintf "UNKNOWN_TYPE"
  | ARROW -> Printf.sprintf "ARROW"
  | OPEN_LIST -> Printf.sprintf "OPEN_LIST"
  | CLOSE_LIST -> Printf.sprintf "CLOSE_LIST"
  | OPEN_BRACKET -> Printf.sprintf "OPEN_BRACKET"
  | CLOSE_BRACKET -> Printf.sprintf "CLOSE_BRACKET"
  | UNKNOWN_TOKEN -> Printf.sprintf "UNKNOWN_TOKEN"
  | EOF -> Printf.sprintf "EOF"
  | _ -> Printf.sprintf "Other"


(* Functions for read the nth line of a file *)
let input_line_opt ic =
 try Some (input_line ic)
 with End_of_file -> None


let nth_line n filename =
 let ic = open_in filename in
 let rec aux i =
   match input_line_opt ic with
   | Some line ->
       if i = n then begin
         close_in ic;
         (line)
       end else aux (succ i)
   | None ->
       close_in ic;
       failwith "end of file reached"
 in
 aux 1


let print_syntax_error env lexbuf =
  let error_color = "\027[38;2;231;29;54m" in
  let position_color = "\027[38;2;0;159;183m" in
  let bold = "\027[1m" in
  let reset = "\027[0m\027[38;2;40;40;40m" in
  let state = MenhirInterpreter.current_state_number env in
  let (start_pos, end_pos) = Sedlexing.lexing_positions lexbuf in
  let start_column_pos = start_pos.pos_cnum - start_pos.pos_bol + 1 in
  let end_column_pos = end_pos.pos_cnum - end_pos.pos_bol + 1 in
  let line = nth_line start_pos.pos_lnum start_pos.pos_fname in
  Printf.fprintf stderr
    "\n%s%sSyntax error%s%s (state %d):%s\n"
    bold error_color reset bold state reset;
  Printf.fprintf stderr
    "%sIn %s %sline %d%s%s, %scolumn %d%s\n"
    bold start_pos.pos_fname
    position_color start_pos.pos_lnum reset bold
    position_color start_column_pos reset;
  Printf.fprintf stderr "\n\t%s\n" line;
  Printf.fprintf stderr
    "\t%s%s%s^%s%s\n\n"
    (String.make (start_column_pos - 1) ' ')
    bold error_color (String.make (end_column_pos - start_column_pos - 1) '~')
    reset;
  Printf.fprintf stderr
    "%s%s%s\n"
    bold (Syntax_error_messages.message state) reset;
  Printf.fprintf stderr "\027[0m";
