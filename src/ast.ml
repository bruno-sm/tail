type type_expression = Int
                     | Real
                     | String
                     | Atom
                     | SpecificAtom of string
                     | Bool
                     | Unknown
                     | Universe of int
                     | Arrow of type_expression * type_expression
                     | Tuple of type_expression list
                     | List of type_expression
                     | Matrix of type_expression
                     | Union of type_expression * type_expression
                     | Intersection of type_expression * type_expression
                     | Complement of type_expression


let rec string_of_type_expression = function
  | Int -> "Int"

  | Real -> "Real"

  | String -> "String"

  | Atom -> "Atom"

  | SpecificAtom s -> Printf.sprintf "SpecificAtom(%s)" s

  | Bool -> "Bool"

  | Unknown -> "Unknown"

  | Universe i -> Printf.sprintf "Universe(%d)" i

  | Arrow (source, target) -> Printf.sprintf "Arrow(%s, %s)"
                              (string_of_type_expression source)
                              (string_of_type_expression target)

  | Tuple types -> Printf.sprintf("Tuple(%s)")
                   (List.map string_of_type_expression types |>
                    List.fold_left (fun s1 s2 -> s1 ^ ", " ^ s2) "")

  | List t -> Printf.sprintf("List(%s)") @@ string_of_type_expression t

  | Matrix t -> Printf.sprintf("Matrix(%s)") @@ string_of_type_expression t

  | Union (t1, t2) -> Printf.sprintf("Union(%s, %s)")
                      (string_of_type_expression t1)
                      (string_of_type_expression t2)

  | Intersection (t1, t2) -> Printf.sprintf("Intersection(%s, %s)")
                             (string_of_type_expression t1)
                             (string_of_type_expression t2)

  | Complement t -> Printf.sprintf("Complement(%s)") @@ string_of_type_expression t


type expression = Sequence of expression list
                | Parentheses of expression
                | Block of expression
                | Variable of string
                | Lambda of string * type_expression * expression
                | FunctionCall of expression * expression
                | Declaration of string * type_expression
                | Assignment of string * expression
                | If of expression * expression *
                        expression list *
                        expression
                | Elif of expression * expression
                | Else of expression
                | Type of type_expression
                | IntLiteral of int
                | RealLiteral of float
                | StringLiteral of string
                | AtomLiteral of string
                | BoolLiteral of bool
                | TupleLiteral of expression list
                | ListLiteral of expression list
                | MatrixLiteral of expression list list


let rec string_of_expression ident = function
  | Sequence exp_list -> Printf.sprintf "Sequence\n%s| %s\n"
                         ident
                         (List.map (string_of_expression (ident ^ " ")) exp_list |>
                          List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | Parentheses e -> Printf.sprintf "Parenthesis\n%s| %s\n"
                     ident
                     (string_of_expression (ident ^ " ") e)

  | Block e -> Printf.sprintf "Block\n%s| %s\n"
               ident
               (string_of_expression (ident ^ " ") e)

  | Variable v -> Printf.sprintf "Variable\n%s| %s\n" ident v

  | Lambda (v, t, e) -> Printf.sprintf "Lambda(%s)\n%s| %s\n%s| %s\n"
                        v
                        ident
                        (string_of_type_expression t)
                        ident
                        (string_of_expression (ident ^ " ") e)

  | FunctionCall (funct, arg) -> Printf.sprintf "FunctionCall\n%s| %s\n%s| %s\n"
                                 ident
                                 (string_of_expression (ident ^ " ") funct)
                                 ident
                                 (string_of_expression (ident ^ " ") arg)

  | Declaration (name, t) -> Printf.sprintf "Declaration(%s)\n%s| %s\n"
                             name
                             ident
                             (string_of_type_expression t)

  | Assignment (name, e) -> Printf.sprintf "Assignment(%s)\n%s| %s\n"
                            name
                            ident
                            (string_of_expression (ident ^ " ") e)

  | If (cond, do_if, [], do_else) -> Printf.sprintf "If\n%s| %s\n%s| %s\n%s| %s\n"
                                    ident
                                    (string_of_expression (ident ^ " ") cond)
                                    ident
                                    (string_of_expression (ident ^ " ") do_if)
                                    ident
                                    (string_of_expression (ident ^ " ") do_else)

  | If (cond, do_if, elif_list, do_else) -> Printf.sprintf "If\n%s| %s\n%s| %s\n%s| %s \n%s| %s\n"
                                            ident
                                            (string_of_expression (ident ^ " ") cond)
                                            ident
                                            (string_of_expression (ident ^ " ") do_if)
                                            ident
                                            (List.map (string_of_expression (ident ^ " ")) elif_list |>
                                             List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")
                                            ident
                                            (string_of_expression (ident ^ " ") do_else)

  | Elif (cond, do_if) -> Printf.sprintf "Elif\n%s| %s\n%s| %s\n"
                          ident
                          (string_of_expression (ident ^ " ") cond)
                          ident
                          (string_of_expression (ident ^ " ") do_if)

  | Else do_else -> Printf.sprintf "Else\n%s| %s\n"
                    ident
                    (string_of_expression (ident ^ " ") do_else)

  | Type t -> Printf.sprintf "Type\n%s| %s\n"
              ident
              (string_of_type_expression t)

  | IntLiteral n -> Printf.sprintf "IntLiteral\n%s| %d\n" ident n

  | RealLiteral x -> Printf.sprintf "RealLiteral\n%s| %f\n" ident x

  | StringLiteral s -> Printf.sprintf "StringLiteral\n%s| %s\n" ident s

  | AtomLiteral a -> Printf.sprintf "AtomLiteral\n%s| %s\n" ident a

  | BoolLiteral true -> Printf.sprintf "BoolLiteral\n%s| True\n" ident

  | BoolLiteral false ->  Printf.sprintf "BoolLiteral\n%s| False\n" ident

  | TupleLiteral exp_list -> Printf.sprintf "TupleLiteral\n%s| %s\n"
                             ident
                             (List.map (string_of_expression (ident ^ " ")) exp_list |>
                              List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | ListLiteral exp_list -> Printf.sprintf "ListLiteral\n%s| %s\n"
                            ident
                            (List.map (string_of_expression (ident ^ " ")) exp_list |>
                             List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")

  | MatrixLiteral exp_matrix -> Printf.sprintf "MatrixLiteral\n%s| %s\n"
                                ident
                                (List.map (List.map (string_of_expression (ident ^ " "))) exp_matrix |>
                                 List.map (List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "") |>
                                 List.fold_left (fun s1 s2 -> s1 ^ "\n" ^ ident ^ "| " ^ s2) "")


let string_of_expression exp = string_of_expression "" exp
