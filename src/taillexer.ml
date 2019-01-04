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

    | _ -> context.check_identation <- true; lexer context lexbuf


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

    | ",," -> SPACE

    | "if" -> IF

    | "then" -> THEN

    | "elif" -> ELIF

    | "else" -> ELSE

    | "or" -> UNION

    | "and" -> INTERSECTION

    | "not" -> COMPLEMENT

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

    | "|" -> MATRIX_SEPARATOR

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


let rec show_lexing' context lexbuf =
  let token = lexer context lexbuf in
  match token with
  | EOF -> Printf.printf "%s " @@ string_of_token token
  | _ ->
    Printf.printf "%s\n" @@ string_of_token token;
    show_lexing' context lexbuf


let show_lexing lexbuf = show_lexing' (new_context ()) lexbuf
