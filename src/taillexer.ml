open Sedlexing
open Tailparser



let digit = [%sedlex.regexp? '0'..'9']

let number = [%sedlex.regexp? Plus digit]

let decimal = [%sedlex.regexp? number, '.', number]

let name = [%sedlex.regexp? Compl (':' | ',' | ';' | '(' | ')' | '<' | '>' | '[' | ']' | white_space | uppercase | digit),
                            Star (Compl (':' | ',' | ';' | '(' | ')' | '<' | '>' | '[' | ']' | white_space | uppercase))]

let name_type = [%sedlex.regexp? Compl (':' | ',' | ';' | '(' | ')' | '<' | '>' | '[' | ']' | white_space | lowercase | digit),
                                 Star (Compl (':' | ',' | ';' | '(' | ')' | '<' | '>' | '[' | ']' | white_space))]

let atom = [%sedlex.regexp? ':', name]

let atom_type = [%sedlex.regexp? ':', name_type]

let universe_type = [%sedlex.regexp? "U", number]



let rec lexer lexbuf =
  match%sedlex lexbuf with
    | number -> INT_NUMBER (Utf8.lexeme lexbuf |> int_of_string)

    | decimal -> REAL_NUMBER (Utf8.lexeme lexbuf |> float_of_string)

    | ":=" -> ASSIGN

    | atom -> let s = Utf8.lexeme lexbuf in
              ATOM (String.sub s 1 ((String.length s) - 1))

    | ';' -> SEQUENCE

    | '(' -> OPEN_PARENTHESES

    | ')' -> CLOSE_PARENTHESES

    | "lambda" -> LAMBDA

    | ':' -> COLON

    | ',' -> COMMA

    | "if" -> IF

    | "elif" -> ELIF

    | "else" -> ELSE

    | "|" -> UNION

    | "&" -> INTERSECTION

    | "-" -> COMPLEMENT

    | "Int" -> INT_TYPE

    | "Real" -> REAL_TYPE

    | "String" -> STRING_TYPE

    | "Atom" -> ATOM_TYPE

    | atom_type -> let s = Utf8.lexeme lexbuf in
                   SPECIFIC_ATOM_TYPE (String.sub s 1 ((String.length s) - 1))

    | "List" -> LIST_TYPE

    | "Matrix" -> MATRIX_TYPE

    | "U" -> UNIVERSE_TYPE 1

    | universe_type -> let s = Utf8.lexeme lexbuf in
                       UNIVERSE_TYPE (String.sub s 1 ((String.length s) - 1) |> int_of_string)

    | "?" -> UNKNOWN_TYPE

    | "->" -> ARROW

    | "<" -> OPEN_LIST

    | ">" -> CLOSE_LIST

    | "[" -> OPEN_BRACKET

    | "]" -> CLOSE_BRACKET

    | name -> NAME (Utf8.lexeme lexbuf)

    | white_space -> lexer lexbuf

    | eof -> EOF

    | any -> UNKNOWN_TOKEN

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
  | NAME n -> Printf.printf "NAME(%s) " n; show_lexing lexbuf
  | ATOM a -> Printf.printf "ATOM(%s) " a; show_lexing lexbuf
  | SEQUENCE -> Printf.printf "SEQUENCE "; show_lexing lexbuf
  | OPEN_PARENTHESES -> Printf.printf "OPEN_PARENTHESES "; show_lexing lexbuf
  | CLOSE_PARENTHESES -> Printf.printf "CLOSE_PARENTHESES "; show_lexing lexbuf
  | LAMBDA -> Printf.printf "LAMBDA "; show_lexing lexbuf
  | COLON -> Printf.printf "COLON "; show_lexing lexbuf
  | ASSIGN -> Printf.printf "ASSIGN "; show_lexing lexbuf
  | COMMA -> Printf.printf "COMMA "; show_lexing lexbuf
  | IF -> Printf.printf "IF "; show_lexing lexbuf
  | ELIF -> Printf.printf "ELIF "; show_lexing lexbuf
  | ELSE -> Printf.printf "ELSE "; show_lexing lexbuf
  | UNION -> Printf.printf "UNION "; show_lexing lexbuf
  | INTERSECTION -> Printf.printf "INTERSECTION "; show_lexing lexbuf
  | COMPLEMENT -> Printf.printf "COMPLEMENT "; show_lexing lexbuf
  | INT_TYPE -> Printf.printf "INT_TYPE "; show_lexing lexbuf
  | REAL_TYPE -> Printf.printf "REAL_TYPE "; show_lexing lexbuf
  | STRING_TYPE -> Printf.printf "STRING_TYPE "; show_lexing lexbuf
  | ATOM_TYPE -> Printf.printf "ATOM_TYPE "; show_lexing lexbuf
  | SPECIFIC_ATOM_TYPE a -> Printf.printf "SPECIFIC_ATOM_TYPE(%s) " a; show_lexing lexbuf
  | LIST_TYPE -> Printf.printf "LIST_TYPE "; show_lexing lexbuf
  | MATRIX_TYPE -> Printf.printf "MATRIX_TYPE "; show_lexing lexbuf
  | UNIVERSE_TYPE n -> Printf.printf "UNIVERSE_TYPE(%d) " n; show_lexing lexbuf
  | UNKNOWN_TYPE -> Printf.printf "UNKNOWN_TYPE "; show_lexing lexbuf
  | ARROW -> Printf.printf "ARROW "; show_lexing lexbuf
  | OPEN_LIST -> Printf.printf "OPEN_LIST "; show_lexing lexbuf
  | CLOSE_LIST -> Printf.printf "CLOSE_LIST "; show_lexing lexbuf
  | OPEN_BRACKET -> Printf.printf "OPEN_BRACKET "; show_lexing lexbuf
  | CLOSE_BRACKET -> Printf.printf "CLOSE_BRACKET "; show_lexing lexbuf
  | UNKNOWN_TOKEN -> Printf.printf "UNKNOWN_TOKEN "; show_lexing lexbuf
  | EOF -> Printf.printf "\n";
  | _ -> Printf.printf "Other "; show_lexing lexbuf
