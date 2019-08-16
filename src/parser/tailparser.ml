open MParser
open Ast



let identation_level = ref 0


let rec count_matches p ?(x = 0) =
  try_skip p >>= fun success -> if success then count_matches p ~x:(x+1) else return x


let whitespace =
  skip_many_chars blank

(* Not valid characters in a name *)
let not_valid_name_chars =
  "+-/\":,.;()<>[]{}|\t\n "


let reserved_words =
  [
    "if";
    "elif";
    "else";
    "then";
    "match";
    "with";
    "typeof";
    "not";
    "and";
    "or";
    "xor";
    "variant";
    "lambda";
  ]


let name =
  look_ahead (is_not uppercase)
  >> many1_chars (none_of not_valid_name_chars)
  >>= fun n -> if List.exists (fun rw -> rw = n) reserved_words then
                fail (Printf.sprintf "The reserved word %s can't be a name." n)
               else
                return n


let name_type s = (
  uppercase
  >>= fun first -> many_chars (none_of not_valid_name_chars)
  >>= fun rest -> return ((String.make 1 first) ^ rest)
)s


let variable =
  name |>> fun n -> Variable n


let atom_literal =
  char ':'
  >>= fun _ -> name
  >>= fun n -> return (AtomLiteral n)


(* Slightly modified version of make_expr_parser to resolve issues with operators inside lists *)
let make_expr_parser' term (ops: ('a, 's) MParser.operator list) : ('a, 's) MParser.t =
  let split_op (rassoc, lassoc, nassoc, prefix, postfix) op =
    match op with
      | Infix (p, Assoc_right) ->
          (p :: rassoc, lassoc, nassoc, prefix, postfix)
      | Infix (p, Assoc_left) ->
          (rassoc, p :: lassoc, nassoc, prefix, postfix)
      | Infix (p, Assoc_none) ->
          (rassoc, lassoc, p :: nassoc, prefix, postfix)
      | Prefix p ->
          (rassoc, lassoc, nassoc, p :: prefix, postfix)
      | Postfix p ->
          (rassoc, lassoc, nassoc, prefix, p :: postfix)
  in

  let (rassoc, lassoc, nassoc, prefix, postfix) =
    List.fold_left split_op ([], [], [], [], []) ops
  in

  let rassoc_op = choice rassoc in
  let lassoc_op = choice lassoc in
  let nassoc_op = choice nassoc in
  let prefix_op = choice prefix in
  let postfix_op = choice postfix in
  let prefix_p = opt (fun x -> x) (attempt (prefix_op << whitespace)) in
  let postfix_p = opt (fun x -> x) (attempt (whitespace >> postfix_op)) in

  let term_p =
    prefix_p >>= fun pre ->
    term >>= fun x ->
    postfix_p >>= fun post ->
    return (post (pre x))
  in

  let rec rassoc_p x =
    (whitespace >> rassoc_op << whitespace) >>= fun f ->
    (term_p >>= (fun z -> rassoc_p' z)) >>= fun y ->
    return (f x y)
  and rassoc_p' x =
    attempt (rassoc_p x) <|> return x
  in

  let rec lassoc_p x =
    (whitespace >> lassoc_op << whitespace) >>= fun f ->
    term_p >>= fun y ->
    lassoc_p' (f x y)
  and lassoc_p' x =
    attempt (lassoc_p x) <|> return x
  in

  let nassoc_p x =
    (whitespace >> nassoc_op << whitespace) >>= fun f ->
    term_p >>= fun y ->
    return (f x y)
  in

  term_p >>= fun x ->
  (attempt (rassoc_p x)
  <|> attempt (lassoc_p x)
  <|> attempt (nassoc_p x)
  <|> return x)

let expression' operators term =
  List.fold_left make_expr_parser' term operators


let infix p op =
  Infix (p |>> (fun _ a b -> (BinOp (op, a, b))), Assoc_left)


let prefix p op =
  Prefix (p |>> (fun _ a -> (UnOp (op, a))))


let postfix p op =
  Postfix (p |>> (fun _ a -> (UnOp (op, a))))


let decimal =
  many1_chars digit


let int_literal =
  decimal |>> fun s -> IntLiteral (int_of_string s)


let real_literal =
  decimal
  >>= fun p1 -> char '.'
  >>= fun _ -> decimal
  >>= fun p2 -> return (RealLiteral (float_of_string (p1 ^ "." ^ p2)))


let number_literal =
   attempt real_literal
   <|> int_literal


let rec type_expression s = (
  let int_type = string "Int" >>$ Int in
  let real_type = string "Real" >>$ Real in
  let string_type = string "String" >>$ String in
  let atom_type = string "Atom" >>$ Atom in
  let specific_atom_type = char ':' >> name_type |>> fun n -> SpecificAtom n in
  let bool_type = string "Bool" >>$ Bool in
  let unknown_type = char '?' >>$ Unknown in
  let void_type = string "Void" >>$ Void in
  let universe_type = char 'U' >> decimal |>> fun i -> Universe (int_of_string i) in
  let variant_type = name_type >>= fun v -> return (Variant v) in
  let list_type =
    attempt (
      (string "List of" >> whitespace >> type_expression |>> fun t -> List t)
    ) <|> (string "List" >>$ List Unknown) in
  let vector_type =
    attempt (
      (string "Vector of" >> whitespace >> type_expression |>> fun t -> Vector t)
    ) <|> (string "Vector" >>$ Vector Unknown) in
  let matrix_type =
    attempt (
      (string "Matrix of" >> whitespace >> type_expression |>> fun t -> Matrix t)
    ) <|> (string "Matrix" >>$ Matrix Unknown) in
  let rec dictionary_type s = (
    let valid_dictionary_type_expression =
      attempt compound_type <|> basic_types
    in
    attempt (
      (string "Dictionary of" >> whitespace >> valid_dictionary_type_expression
       >>= fun t1 -> whitespace >> char ',' >> whitespace >> valid_dictionary_type_expression
       >>= fun t2 -> return (Dictionary (t1, t2)))
    ) <|> (string "Dictionary" >>$ Dictionary (Unknown, Unknown))
  )s
  and parentheses_type s = (
    between (char '(') (char ')') (whitespace >> type_expression << whitespace)
    |>> fun e -> ParenthesesType e
  )s
  and basic_types s = (
    choice [
      attempt int_type;
      attempt real_type;
      attempt string_type;
      attempt atom_type;
      attempt specific_atom_type;
      attempt bool_type;
      attempt unknown_type;
      attempt void_type;
      attempt universe_type;
      attempt list_type;
      attempt vector_type;
      attempt matrix_type;
      attempt dictionary_type;
      attempt parentheses_type;
      variant_type;
    ])s

  and compound_type s = (
    let type_operators =
      [
        [
          Prefix (string "not" |>> (fun _ a-> (Complement a)))
        ];
        [
          Infix (string "and" |>> (fun _ a b -> (Intersection (a, b))), Assoc_left)
        ];
        [
          Infix (string "or" |>> (fun _ a b -> (Union (a, b))), Assoc_left)
        ];
        [
          Infix (string "->" |>> (fun _ a b -> (Arrow (a, b))), Assoc_left);
        ];
      ] in
    let valid_compound_type_expression =
      attempt (tuple_type ~allow_compound:false)
      <|> basic_types
    in
    expression' type_operators valid_compound_type_expression
  )s
  and tuple_type ?(allow_compound = true) s = (
    let valid_tuple_type_expression =
      if allow_compound then
        (attempt compound_type <|> basic_types)
      else basic_types
    in
    (whitespace >> valid_tuple_type_expression << whitespace)
    >>= fun first_exp -> char ','
    >>= fun _ -> sep_by1 (whitespace >> valid_tuple_type_expression << whitespace) (char ',')
    >>= fun exp_list -> return (Tuple (first_exp::exp_list))
  )s in
  attempt tuple_type
  <|> attempt compound_type
  <|> basic_types
)s


let type_annotation =
  name >>= fun n -> whitespace >> char ':' >> whitespace
  >> type_expression >>= fun t -> return (Annotation (n, t))


let variant_declaration =
  let constructor_list =
    let constructor =
      let arg_list =
        char '(' >> whitespace
        >> (sep_by1 (whitespace >> name << whitespace) (char ','))
        >>= fun a -> whitespace >> char ')' >> return a
      in
      let type_ann =
        char ':' >> whitespace >> type_expression
      in
      name_type >>= fun n -> whitespace >> (attempt arg_list <|> return [])
      >>= fun a -> whitespace >> (attempt type_ann <|> return Unknown)
      >>= fun t -> return {name = n; arguments = a; argument_type = t}
    in
    sep_by1 (whitespace >> constructor << whitespace) (char '|')
  in
  string "variant" >> blank >> whitespace
  >> name_type >>= fun v -> whitespace >> string "::" >> whitespace >> constructor_list
  >>= fun c -> return (VariantDeclaration (v, c))


let rec basic_expr s = (
  choice [
  variant_declaration;
  match_exp;
  conditional;
  lambda;
  atom_literal;
  parentheses;
  string_literal;
  number_literal;
  attempt method_ss;
  attempt variant_projection;
  attempt variant_instance;
  attempt function_declaration_and_annotation_ss;
  attempt function_declaration_ss;
  attempt function_call;
  attempt list_decomposition;
  list_literal;
  attempt matrix_literal;
  vector_literal;
  attempt assign;
  attempt assign_and_annotation_ss;
  attempt type_annotation;
  variable;
  ]
)s


and arithmetic_operators () =
  [
    [
      Postfix (between (char '[') (char ']')
                       (whitespace >> expr << whitespace)
               |>> (fun p e -> (UnOp (Projection p, e))));
    ];
    [
      infix (string "//") Frac;
    ];
    [
      postfix (char 'i') I;
    ];
    [
      prefix (char '+') Add;
      prefix (char '-') Sub;
    ];
    [
      infix (char '^') Exp;
    ];
    [
      infix (char '*') Mul;
      infix (char '/') Div;
      infix (char '%') Rem;
    ];
    [
      infix (char '+') Add;
      infix (char '-') Sub;
    ];
    [
      infix (char '<') Less;
      infix (char '>') Greater;
      infix (string "<=") LessEqual;
      infix (string ">=") GreaterEqual;
    ];
    [
      infix (char '=') Equal;
      infix (string "!=") Different;
    ];
    [
      infix (string "and") And;
    ];
    [
      infix (string "xor") Xor;
    ];
    [
      infix (string "or") Or;
    ];
    [
      prefix (string "typeof") Typeof;
    ];
  ]


and arithmetic_expr s = (
  attempt (expression' (arithmetic_operators ()) basic_expr)
)s


and assign s = (
  name << whitespace
  >>= fun n -> string ":=" >> whitespace
  >> expr ~newline_seqs:false
  >>= fun e -> return (Assignment (false, n, e))
)s


and assign_and_annotation_ss s = (
  name >>= fun n -> whitespace >> char ':' >> whitespace
  >> type_expression >>= fun t -> whitespace >> string ":=" >> whitespace
  >> expr ~newline_seqs:false >>= fun e -> return (Sequence [Annotation (n, t); Assignment (false, n, e)])
)s


and argument_list s = (
  sep_end_by (whitespace >> name << whitespace) (char ',')
)s


and lambda s = (
  let type_ann =
    option (char ':' >> whitespace >> type_expression)
    >>= fun opt_type -> match opt_type with
                          | Some t -> return t
                          | None -> return (Arrow(Unknown, Unknown))
  in
  string "lambda" >> whitespace >> argument_list
  >>= fun arg -> whitespace >> type_ann
  >>= fun t -> whitespace >> char '.' >> whitespace >> expr
  >>= fun e -> return (Lambda (arg, t, e))
)s


and function_declaration_ss s = (
  name << whitespace
  >>= fun n -> char '(' >> argument_list
  >>= fun arg -> char ')' >> whitespace
  >> string ":=" >> whitespace >> expr ~newline_seqs:false
  >>= fun e -> return (Assignment (true, n, Lambda (arg, Arrow(Unknown, Unknown), e)))
)s


and function_declaration_and_annotation_ss s = (
  name << whitespace
  >>= fun n -> char '(' >> argument_list
  >>= fun arg -> char ')' >> whitespace
  >> char ':' >> whitespace >> type_expression
  >>= fun t -> whitespace >> string ":=" >> whitespace >> expr ~newline_seqs:false
  >>= fun e -> return (Sequence [Annotation (n, t); Assignment (true, n, Lambda (arg, t, e))])
)s


and function_call s = (
  let arg =
    between (char '(') (char ')') (whitespace >> expr << whitespace)
  in
  let rec f_call e =
    whitespace >> arg
    >>= fun a ->
    f_call' (FunctionCall (e, Some a))
  and f_call' e =
    attempt (f_call e) <|> return e
  in
  (attempt parentheses <|> variable)
  >>= fun e -> f_call e
)s


and variant_instance s = (
  let arg =
    between (char '(') (char ')') (whitespace >> expr << whitespace)
  in
  name_type
  >>= fun v -> string "::" >> name_type
  >>= fun c -> whitespace >> option arg
  >>= fun a -> return (VariantInstance (v, c, a))
)s


and variant_projection s = (
  let valid_expression =
    attempt parentheses
    <|> attempt function_call
    <|> attempt variant_instance
    <|> variable
  in
  valid_expression >>= fun e -> char '.' >> name
  >>= fun n -> return (VariantProjection (e, n))
)s


and method_ss s = (
  let valid_expression =
    attempt parentheses
    <|> attempt function_call
    <|> attempt variant_instance
    <|> variable
  in
  let arg =
    between (char '(') (char ')') (whitespace >> option expr << whitespace)
  in
  valid_expression >>= fun e -> char '.' >> variable
  >>= fun f -> whitespace >> arg
  >>= fun opt_a -> match opt_a with
  | Some (TupleLiteral a) -> return (FunctionCall (f, Some (TupleLiteral (e::a))))
  | Some a -> return (FunctionCall (f, Some (TupleLiteral [e; a])))
  | None -> return (FunctionCall (f, Some (e)))
)s


and conditional s = (
  let elif =
    string "elif" >> whitespace >> expr
    >>= fun cond -> whitespace >> string "then" >> whitespace >> expr
    >>= fun body -> return (Elif (cond, body))
  in
  let _else =
    string "else" >> whitespace >> expr ~newline_seqs:false
    >>= fun body -> return (Else body)
  in
  string "if" >> whitespace >> expr
  >>= fun if_cond -> whitespace >> string "then" >> whitespace >> expr
  >>= fun if_body -> whitespace >> many newline >> many elif
  >>= fun elif_list -> whitespace >> many newline >> option _else
  >>= fun else_opt -> return (If (if_cond, if_body, elif_list, else_opt))
)s


and match_exp s = (
  let pattern_expr =
    attempt (char '_' |>> fun _ -> AnyMatch)
    <|> expr
  in
  let match_option =
    whitespace >> char '|' >> whitespace >> pattern_expr
    >>= fun pattern -> whitespace >> string "->" >> whitespace >> expr
    >>= fun e -> return (pattern, e)
  in
  string "match" >> whitespace >> expr
  >>= fun m -> whitespace >> string "with" >> many1 (attempt match_option)
  >>= fun o -> return (Match (m, o))
)s


and list_literal s = (
  char '<'
  >> whitespace
  >> (sep_end_by expr (many1 blank))
  >>= fun exp_list -> char '>' >> return (ListLiteral exp_list)
)s


and list_decomposition s = (
  char '<' >> whitespace >> name
  >>= fun h -> whitespace >> char '|' >> whitespace >> name
  >>= fun t -> whitespace >> char '>' >> return (ListDecomposition (h, t))
)s


and vector_literal s = (
  char '['
  >> whitespace
  >> (sep_end_by expr (many1 blank))
  >>= fun exp_list -> char ']' >> return (VectorLiteral exp_list)
)s


and matrix_literal s = (
  let row = (sep_end_by (whitespace >> expr) (many1 blank)) in
  let sep = char '|' >>$ optional (char '|') in
  char '['
  >> row
  >>= fun first -> sep
  >> sep_by1 row sep
  >>= fun rest -> char ']'
  >>$ MatrixLiteral (first::rest)
)s


and valid_tuple_expression s = (
  attempt arithmetic_expr
  <|> basic_expr
)s


and tuple_literal s = (
  (whitespace >> valid_tuple_expression << whitespace)
  >>= fun first_exp -> char ','
  >>= fun _ -> sep_by1 (whitespace >> valid_tuple_expression << whitespace) (char ',')
  >>= fun exp_list -> return (TupleLiteral (first_exp::exp_list))
)s


and dictionay_literal s = (
  let valid_dictionary_expression =
    attempt arithmetic_expr
    <|> basic_expr
  in
  let key_value_pair =
    valid_dictionary_expression >>=
    fun k -> whitespace >> string "=>" >> whitespace >>
    valid_dictionary_expression >>=
    fun v -> return (k, v)
  in
  sep_by1 key_value_pair (whitespace >> char ',' << whitespace)
  |>> fun p_list -> DictionaryLiteral (p_list)
)s


and string_literal s = (
  let embedded_expression =
    whitespace >> expr >>= fun e -> whitespace >> char '}' >> return e
  in
  let escaped_char =
  any_of "nrtb\\\"\'" |>> (function
     | 'n' -> '\n'
     | 'r' -> '\r'
     | 't' -> '\t'
     | 'b' -> '\b'
     | c -> c)
  in
  let char_token =
      (char '\\' >> escaped_char)
      <|> any_char
  in
  let rec string_content x =
    (char '"' <|> char '{')
    >>= fun c -> match c with
          | '{' -> embedded_expression
                   >>= fun e -> string_content' (BinOp (Add,
                                                       x,
                                                       FunctionCall(Variable("to_string"),
                                                                    Some e)))
          | _ -> return x
  and string_content' x =
    (many_chars (not_followed_by (char '"' <|> char '{') "" >> char_token))
    >>= fun str -> string_content (BinOp (Add, x, StringLiteral str))
  in
  char '"' >> (many_chars (not_followed_by (char '"' <|> char '{') "" >> char_token))
  >>= fun str -> string_content (StringLiteral str)
)s


and sequence ?(ident_level = 0) ?(allow_newline = true) s = (
  let valid_sequence_expression =
    attempt (block ident_level)
    <|> attempt dictionay_literal
    <|> attempt tuple_literal
    <|> attempt arithmetic_expr
    <|> basic_expr
  in
  let seq_sep =
    if allow_newline then
      attempt (skip (many1 newline >> count ident_level tab))
      <|> skip (char ';')
    else
      skip (char ';')
  in
  let rec seq_content seq_list =
    attempt ((seq_sep >> many (char ' ') >> valid_sequence_expression << whitespace)
    >>= fun e -> seq_content (e::seq_list))
    <|> (if (List.length seq_list) < 2 then fail "" else return seq_list)
  in
  (many (char ' ') >> valid_sequence_expression << whitespace)
  >>= fun first_exp -> seq_content [first_exp]
  >>= fun exp_list -> return (Sequence (List.rev exp_list))
)s


and block current_ident_level s = (
  many newline >> count_matches tab
  >>= fun il -> if il = 0 then fail "" else
  expr ~ident_level:(current_ident_level + il) >>= fun e -> return (Block e)
)s


and parentheses s = (
  between (char '(') (char ')') (whitespace >> expr << whitespace)
  |>> fun e -> Parentheses e
)s


and expr ?(ident_level = 0) ?(newline_seqs = true) s = (
  attempt (sequence ~ident_level:ident_level ~allow_newline:newline_seqs)
  <|> attempt (block ident_level)
  <|> attempt dictionay_literal
  <|> attempt tuple_literal
  <|> attempt arithmetic_expr
  <|> attempt basic_expr
)s


let parse (inc : in_channel) : (expression, (pos * string)) Result.result =
  match MParser.parse_channel (spaces >> expr << spaces << eof) inc () with
    | Success ast ->
        Result.Ok ast
    | Failed (msg, e) -> match e with
                         | Parse_error (pos, _) -> Result.Error (pos, msg)
                         | No_error -> Result.Error ((0, 0, 0), msg)


let parse_string (s : string) : unit =
  match MParser.parse_string expr s () with
    | Success ast ->
        string_of_expression ast |> print_endline
    | Failed (msg, e) ->
        print_endline msg


(* Prints pretty error messages *)
let print_syntax_error ic pos msg =
  let error_color = "\027[38;2;231;29;54m" in
  let position_color = "\027[38;2;0;159;183m" in
  let bold = "\027[1m" in
  let reset = "\027[0m\027[38;2;40;40;40m" in
  let (index, line_num, column_num) = pos in
  let line = seek_in ic (index - column_num + 1); input_line ic in
  let msg = List.nth (String.split_on_char '\n' msg) 3 in
  Printf.fprintf stderr
    "\n%s%sSyntax error:%s\n"
    bold error_color reset;
  Printf.fprintf stderr
    "%sIn %sline %d%s%s, %scolumn %d%s\n"
    bold
    position_color line_num reset bold
    position_color column_num reset;
  Printf.fprintf stderr "\n\t%s\n" line;
  Printf.fprintf stderr
    "\t%s%s%s^%s\n\n"
    (String.make (column_num - 1) ' ')
    bold error_color
    reset;
  Printf.fprintf stderr
    "%s%s%s\n\n"
    bold msg reset;
  Printf.fprintf stderr "\027[0m";
