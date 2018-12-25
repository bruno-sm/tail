open Sedlexing
open Tailparser



let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let decimal = [%sedlex.regexp? number, '.', number]


let rec lexer lexbuf =
  match%sedlex lexbuf with
    | white_space -> lexer lexbuf
    | number -> INT_NUMBER (Utf8.lexeme lexbuf |> int_of_string)
    | decimal -> REAL_NUMBER (Utf8.lexeme lexbuf |> float_of_string)
    | eof -> EOF
    | _ -> UNKNOWN_TOKEN


let lexer_for_menhir (sedlexbuf : Sedlexing.lexbuf) (lexbuf : Lexing.lexbuf) =
  let (token, start_pos, curr_pos) = (Sedlexing.with_tokenizer lexer sedlexbuf) () in
  lexbuf.lex_start_p <- start_pos;
  lexbuf.lex_curr_p <- curr_pos;
  token


let rec show_lexing lexbuf =
  match lexer lexbuf with
  | INT_NUMBER n -> Printf.printf "INT_NUMBER(%d) " n; show_lexing lexbuf
  | REAL_NUMBER x -> Printf.printf "REAL_NUMBER(%f) " x; show_lexing lexbuf
  | UNKNOWN_TOKEN -> Printf.printf "UNKNOWN_TOKEN "; show_lexing lexbuf
  | EOF -> Printf.printf "\n";
  | _ -> Printf.printf "Other "; show_lexing lexbuf
