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


type context = {
  mutable token_number : int;
  mutable check_identation : bool;
  mutable ident_depth : int
}

let new_context () = {token_number = 0; check_identation = true; ident_depth = 0}


(* This function is called after finding a breakline *)
let rec skip_breaklines context lexbuf =
  match%sedlex lexbuf with
    | Sub (white_space, "\t") -> skip_breaklines context lexbuf

    | eof -> lexer context lexbuf

    | _ -> context.check_identation <- true; SEQUENCE


and lexer context lexbuf =
  (* Ignores initial breaklines *)
  if context.token_number = 0 then
    match%sedlex lexbuf with
    | Star (Sub (white_space, "\t")) -> ()
    | _ -> ()
  else ();

  (* Checks the identation level *)
  if context.check_identation then begin
    context.check_identation <- false;
    match%sedlex lexbuf with
    | Star "\t" -> let ident_depth = String.length (Utf8.lexeme lexbuf) in
                   let ident_diff = ident_depth - context.ident_depth in
                   context.ident_depth <- ident_depth;
                   if ident_diff > 0 then BLOCK_BEGIN ident_diff
                   else if ident_diff < 0 then BLOCK_END ident_diff
                   else lexer context lexbuf
    | _ -> lexer context lexbuf
  end else

  let token = match%sedlex lexbuf with
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

    | "\n" -> skip_breaklines context lexbuf

    | white_space -> lexer context lexbuf

    | name -> NAME (Utf8.lexeme lexbuf)

    | eof -> if context.ident_depth > 0 then
              let ident_diff = -context.ident_depth in
              context.ident_depth <- 0;
              BLOCK_END ident_diff
             else EOF

    | any -> UNKNOWN_TOKEN

    | _ -> UNKNOWN_TOKEN
  in
  context.token_number <- context.token_number + 1;
  token


let lexer_for_menhir context (sedlexbuf : Sedlexing.lexbuf) (lexbuf : Lexing.lexbuf) =
  let (token, start_pos, curr_pos) = (Sedlexing.with_tokenizer (lexer context) sedlexbuf) () in
  lexbuf.lex_start_p <- start_pos;
  lexbuf.lex_curr_p <- curr_pos;
  token


let rec show_lexing' context lexbuf =
  match lexer context lexbuf with
  | INT_NUMBER n -> Printf.printf "INT_NUMBER(%d) " n; show_lexing' context lexbuf
  | REAL_NUMBER x -> Printf.printf "REAL_NUMBER(%f) " x; show_lexing' context lexbuf
  | NAME n -> Printf.printf "NAME(%s) " n; show_lexing' context lexbuf
  | ATOM a -> Printf.printf "ATOM(%s) " a; show_lexing' context lexbuf
  | SEQUENCE -> Printf.printf "SEQUENCE "; show_lexing' context lexbuf
  | OPEN_PARENTHESES -> Printf.printf "OPEN_PARENTHESES "; show_lexing' context lexbuf
  | CLOSE_PARENTHESES -> Printf.printf "CLOSE_PARENTHESES "; show_lexing' context lexbuf
  | LAMBDA -> Printf.printf "LAMBDA "; show_lexing' context lexbuf
  | COLON -> Printf.printf "COLON "; show_lexing' context lexbuf
  | BLOCK_BEGIN t -> Printf.printf "BLOCK_BEGIN(%d) " t; show_lexing' context lexbuf
  | BLOCK_END t -> Printf.printf "BLOCK_END(%d) " t; show_lexing' context lexbuf
  | ASSIGN -> Printf.printf "ASSIGN "; show_lexing' context lexbuf
  | COMMA -> Printf.printf "COMMA "; show_lexing' context lexbuf
  | IF -> Printf.printf "IF "; show_lexing' context lexbuf
  | ELIF -> Printf.printf "ELIF "; show_lexing' context lexbuf
  | ELSE -> Printf.printf "ELSE "; show_lexing' context lexbuf
  | UNION -> Printf.printf "UNION "; show_lexing' context lexbuf
  | INTERSECTION -> Printf.printf "INTERSECTION "; show_lexing' context lexbuf
  | COMPLEMENT -> Printf.printf "COMPLEMENT "; show_lexing' context lexbuf
  | INT_TYPE -> Printf.printf "INT_TYPE "; show_lexing' context lexbuf
  | REAL_TYPE -> Printf.printf "REAL_TYPE "; show_lexing' context lexbuf
  | STRING_TYPE -> Printf.printf "STRING_TYPE "; show_lexing' context lexbuf
  | ATOM_TYPE -> Printf.printf "ATOM_TYPE "; show_lexing' context lexbuf
  | SPECIFIC_ATOM_TYPE a -> Printf.printf "SPECIFIC_ATOM_TYPE(%s) " a; show_lexing' context lexbuf
  | LIST_TYPE -> Printf.printf "LIST_TYPE "; show_lexing' context lexbuf
  | MATRIX_TYPE -> Printf.printf "MATRIX_TYPE "; show_lexing' context lexbuf
  | UNIVERSE_TYPE n -> Printf.printf "UNIVERSE_TYPE(%d) " n; show_lexing' context lexbuf
  | UNKNOWN_TYPE -> Printf.printf "UNKNOWN_TYPE "; show_lexing' context lexbuf
  | ARROW -> Printf.printf "ARROW "; show_lexing' context lexbuf
  | OPEN_LIST -> Printf.printf "OPEN_LIST "; show_lexing' context lexbuf
  | CLOSE_LIST -> Printf.printf "CLOSE_LIST "; show_lexing' context lexbuf
  | OPEN_BRACKET -> Printf.printf "OPEN_BRACKET "; show_lexing' context lexbuf
  | CLOSE_BRACKET -> Printf.printf "CLOSE_BRACKET "; show_lexing' context lexbuf
  | UNKNOWN_TOKEN -> Printf.printf "UNKNOWN_TOKEN "; show_lexing' context lexbuf
  | EOF -> Printf.printf "EOF\n";
  | _ -> Printf.printf "Other "; show_lexing' context lexbuf


let show_lexing lexbuf = show_lexing' (new_context ()) lexbuf
