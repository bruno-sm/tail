type info = {
  start_pos : int * int * int;
  end_pos : int * int * int;
  _type : type_expression option;
}

and type_expression = Int
                     | Real
                     | String
                     | Atom
                     | SpecificAtom of string
                     | Bool
                     | Unknown
                     | Void
                     | Universe
                     | Variant of string
                     | Arrow of type_expression * type_expression
                     | Tuple of type_expression list
                     | List of type_expression
                     | Vector of type_expression
                     | Matrix of type_expression
                     | Dictionary of type_expression * type_expression
                     | Union of type_expression * type_expression
                     | Intersection of type_expression * type_expression
                     | Complement of type_expression
                     | ParenthesesType of type_expression


let pos_info s e = {start_pos=s; end_pos=e; _type=None}


let rec string_of_type_expression = function
  | Int -> "Int"

  | Real -> "Real"

  | String -> "String"

  | Atom -> "Atom"

  | SpecificAtom s -> Printf.sprintf "SpecificAtom(%s)" s

  | Bool -> "Bool"

  | Unknown -> "Unknown"

  | Void -> "Void"

  | Universe -> "Universe"

  | Variant v -> Printf.sprintf "Variant(%s)" v

  | Arrow (source, target) -> Printf.sprintf "Arrow(%s, %s)"
                              (string_of_type_expression source)
                              (string_of_type_expression target)

  | Tuple types -> Printf.sprintf("Tuple(%s)")
                   (List.map string_of_type_expression types |>
                    List.fold_left (fun s1 s2 -> s1 ^ ", " ^ s2) "")

  | List t -> Printf.sprintf("List(%s)") @@ string_of_type_expression t

  | Vector t -> Printf.sprintf("Vector(%s)") @@ string_of_type_expression t

  | Matrix t -> Printf.sprintf("Matrix(%s)") @@ string_of_type_expression t

  | Dictionary (t1, t2) -> Printf.sprintf("Dictionary(%s, %s)")
                           (string_of_type_expression t1)
                           (string_of_type_expression t2)

  | Union (t1, t2) -> Printf.sprintf("Union(%s, %s)")
                      (string_of_type_expression t1)
                      (string_of_type_expression t2)

  | Intersection (t1, t2) -> Printf.sprintf("Intersection(%s, %s)")
                             (string_of_type_expression t1)
                             (string_of_type_expression t2)

  | Complement t -> Printf.sprintf("Complement(%s)") @@ string_of_type_expression t

  | ParenthesesType t -> Printf.sprintf("ParenthesesType(%s)") @@ string_of_type_expression t


type expression = Sequence of info * expression list
                | Parentheses of info * expression
                | Block of info * expression
                | BinOp of info * operator * expression * expression
                | UnOp of info * operator * expression
                | Variable of info * string
                | Lambda of info * string list * type_expression * expression
                | FunctionCall of info * expression * expression option
                | Annotation of info * string * type_expression
                | VariantDeclaration of info * string * variant_constructor list
                | VariantInstance of info * string * string * expression option
                | VariantProjection of info * expression * string
                | Assignment of info * bool * string * expression
                | If of info * expression * expression *
                        expression list *
                        expression option
                | Elif of info * expression * expression
                | Else of info * expression
                | Match of info * expression * (expression * expression) list
                | AnyMatch of info
                | IntLiteral of info * int
                | RealLiteral of info * float
                | RationalLiteral of info * int * int
                | ComplexLiteral of info * float * float
                | StringLiteral of info * string
                | AtomLiteral of info * string
                | BoolLiteral of info * bool
                | TupleLiteral of info * expression list
                | ListLiteral of info * expression list
                | ListDecomposition of info * string * string
                | VectorLiteral of info * expression list
                | MatrixLiteral of info * expression list list
                | DictionaryLiteral of info * (expression * expression) list

and operator = Add
             | Sub
             | Mul
             | Div
             | Frac
             | Rem
             | Exp
             | I
             | Typeof
             | And
             | Xor
             | Or
             | Equal
             | Different
             | Less
             | LessEqual
             | Greater
             | GreaterEqual
             | Projection of expression

and variant_constructor = {
  name : string;
  arguments : string list;
  argument_type : type_expression;
}


let rec string_of_expression ident = function
  | Sequence (_, exp_list) -> Printf.sprintf "Sequence\n%s| %s\n"
                         ident
                         (List.map (string_of_expression (ident ^ " ")) exp_list |>
                          List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | Parentheses (_, e) -> Printf.sprintf "Parentheses\n%s| %s\n"
                     ident
                     (string_of_expression (ident ^ " ") e)

  | Block (_, e) -> Printf.sprintf "Block\n%s| %s\n"
               ident
               (string_of_expression (ident ^ " ") e)

  | BinOp (_, op, e1, e2) -> Printf.sprintf "BinOp(%s)\n%s| %s\n%s| %s\n"
                          (string_of_operator ident op)
                          ident
                          (string_of_expression (ident ^ " ") e1)
                          ident
                          (string_of_expression (ident ^ " ") e2)

  | UnOp (_, op, e) -> Printf.sprintf "UnOp(%s)\n%s| %s\n"
                          (string_of_operator ident op)
                          ident
                          (string_of_expression (ident ^ " ") e)

  | Variable (_, v) -> Printf.sprintf "Variable\n%s| %s\n" ident v

  | Lambda (_, args, t, e) -> Printf.sprintf "Lambda(%s)\n%s| %s\n%s| %s\n"
                        (List.fold_left (fun a b -> a ^ ", " ^ b) "" args)
                        ident
                        (string_of_type_expression t)
                        ident
                        (string_of_expression (ident ^ " ") e)

  | FunctionCall (_, funct, arg) -> Printf.sprintf "FunctionCall\n%s| %s\n%s| %s\n"
                                 ident
                                 (string_of_expression (ident ^ " ") funct)
                                 ident
                                 begin match arg with
                                 | Some a -> (string_of_expression (ident ^ " ") a)
                                 | None -> "" end

  | Annotation (_, name, t) -> Printf.sprintf "Annotation(%s)\n%s| %s\n"
                             name
                             ident
                             (string_of_type_expression t)

  | VariantDeclaration (_, name, const_list) -> Printf.sprintf "VariantDeclaration(%s)\n%s| %s\n"
                                             name
                                             ident
                                             (List.map (string_of_variant_constructor ident) const_list |>
                                              List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | VariantInstance (_, vname, cname, arg) -> Printf.sprintf "VariantInstance(%s::%s)\n%s| %s\n"
                                           vname cname ident
                                           begin match arg with
                                           | Some a -> (string_of_expression (ident ^ " ") a)
                                           | None -> "" end

  | VariantProjection (_, variant, label) -> Printf.sprintf "VariantProjection\n%s| %s\n%s| %s\n"
                                          ident
                                          (string_of_expression (ident ^ " ") variant)
                                          ident
                                          label

  | Assignment (_, _, name, e) -> Printf.sprintf "Assignment(%s)\n%s| %s\n"
                            name
                            ident
                            (string_of_expression (ident ^ " ") e)

  | If (_, cond, do_if, [], do_else) -> Printf.sprintf "If\n%s| %s\n%s| %s\n%s| %s\n"
                                    ident
                                    (string_of_expression (ident ^ " ") cond)
                                    ident
                                    (string_of_expression (ident ^ " ") do_if)
                                    ident
                                    begin match do_else with
                                    | Some e -> (string_of_expression (ident ^ " ") e)
                                    | None -> "" end

  | If (_, cond, do_if, elif_list, do_else) -> Printf.sprintf "If\n%s| %s\n%s| %s\n%s| %s \n%s| %s\n"
                                            ident
                                            (string_of_expression (ident ^ " ") cond)
                                            ident
                                            (string_of_expression (ident ^ " ") do_if)
                                            ident
                                            (List.map (string_of_expression (ident ^ " ")) elif_list |>
                                             List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")
                                            ident
                                            begin match do_else with
                                            | Some e -> (string_of_expression (ident ^ " ") e)
                                            | None -> "" end

  | Elif (_, cond, do_if) -> Printf.sprintf "Elif\n%s| %s\n%s| %s\n"
                          ident
                          (string_of_expression (ident ^ " ") cond)
                          ident
                          (string_of_expression (ident ^ " ") do_if)

  | Else (_, do_else) -> Printf.sprintf "Else\n%s| %s\n"
                    ident
                    (string_of_expression (ident ^ " ") do_else)

  | Match (_, m, patterns) -> Printf.sprintf "Match\n%s| %s\n"
                           ident
                           begin let patterns_str =
                            List.map (fun p -> (Printf.sprintf "Pattern\n%s| %s\n" ident (string_of_expression ident (fst p))) ^
                                                 (Printf.sprintf "Expression\n%s| %s\n" ident (string_of_expression ident (snd p))))
                                      patterns
                           in
                           (List.fold_left (fun a b -> a ^ b) "" patterns_str) end

  | AnyMatch _ -> "AnyMatch"

  | IntLiteral (_, n) -> Printf.sprintf "IntLiteral\n%s| %d\n" ident n

  | RealLiteral (_, x) -> Printf.sprintf "RealLiteral\n%s| %f\n" ident x

  | RationalLiteral (_, p, q) -> Printf.sprintf "RationalLiteral\n%s | %d, %d\n" ident p q

  | ComplexLiteral (_, a, b) -> Printf.sprintf "ComplexLiteral\n%s | %f, %f\n" ident a b

  | StringLiteral (_, s) -> Printf.sprintf "StringLiteral\n%s| %s\n" ident s

  | AtomLiteral (_, a) -> Printf.sprintf "AtomLiteral\n%s| %s\n" ident a

  | BoolLiteral (_, true) -> Printf.sprintf "BoolLiteral\n%s| True\n" ident

  | BoolLiteral (_, false) ->  Printf.sprintf "BoolLiteral\n%s| False\n" ident

  | TupleLiteral (_, exp_list) -> Printf.sprintf "TupleLiteral\n%s| %s\n"
                             ident
                             (List.map (string_of_expression (ident ^ " ")) exp_list |>
                              List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | ListLiteral (_, exp_list) -> Printf.sprintf "ListLiteral\n%s| %s\n"
                            ident
                            (List.map (string_of_expression (ident ^ " ")) exp_list |>
                             List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | ListDecomposition (_, h, t) -> Printf.sprintf "ListDecomposition(%s, %s)\n" h t

  | VectorLiteral (_, exp_list) -> Printf.sprintf "VectorLiteral\n%s| %s\n"
                           ident
                           (List.map (string_of_expression (ident ^ " ")) exp_list |>
                            List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | MatrixLiteral (_, exp_matrix) -> Printf.sprintf "MatrixLiteral\n%s| %s\n"
                                ident
                                (List.map (List.map (string_of_expression (ident ^ " "))) exp_matrix |>
                                 List.map (List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "") |>
                                 List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | DictionaryLiteral (_, key_values) -> Printf.sprintf "DictionaryLiteral\n%s| %s\n"
                                    ident
                                    begin let key_values_str =
                                      List.map (fun kv -> Printf.sprintf "KeyValuePair\n%s| %s\n%s| %s\n"
                                                                         ident
                                                                         (string_of_expression ident (fst kv))
                                                                         ident
                                                                         (string_of_expression ident (snd kv)))
                                               key_values
                                    in
                                      List.fold_left (fun a b -> a ^ b) "" key_values_str
                                    end

  | StringLiteral (_, str) -> Printf.sprintf "StringLiteral(%s)\n" str

  | _ -> "Other Expression"

and string_of_operator ident = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Frac -> "Frac"
  | Rem -> "Rem"
  | Exp -> "Exp"
  | I -> "I"
  | Typeof -> "Typeof"
  | And -> "And"
  | Xor -> "Xor"
  | Or -> "Or"
  | Equal -> "Equal"
  | Different -> "Different"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Projection e -> Printf.sprintf "Projection\n%s| %s\n" ident (string_of_expression ident e)
  | _ -> "Other operator"

and string_of_variant_constructor ident c =
  Printf.sprintf "%s(%s) : %s"
  c.name
  (List.fold_left (fun s1 s2 -> s1 ^ "," ^ s2) "" c.arguments)
  (string_of_type_expression c.argument_type)


let string_of_expression exp = string_of_expression "" exp
