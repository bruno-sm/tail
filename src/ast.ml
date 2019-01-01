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
                | FunctionCall of string * expression
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


let rec string_of_expression = function
  | Sequence exp_list -> Printf.sprintf "Sequence\n\t| %s\n"
                         (List.map string_of_expression exp_list |>
                          List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "")

  | Parentheses e -> Printf.sprintf "Parenthesis\n\t| %s\n" @@ string_of_expression e

  | Block e -> Printf.sprintf "Block\n\t| %s\n" @@ string_of_expression e

  | Variable v -> Printf.sprintf "Variable\n\t| %s\n" v

  | Lambda (v, t, e) -> Printf.sprintf "Lambda(%s)\n\t| %s\n\t| %s\n"
                        v
                        (string_of_type_expression t)
                        (string_of_expression e)

  | FunctionCall (name, arg) -> Printf.sprintf "FunctionCall(%s)\n\t| %s\n"
                                name
                                (string_of_expression arg)

  | Declaration (name, t) -> Printf.sprintf "Declaration(%s)\n\t| %s\n"
                             name
                             (string_of_type_expression t)

  | Assignment (name, e) -> Printf.sprintf "Assignment(%s)\n\t| %s\n"
                            name
                            (string_of_expression e)

  | If (cond, do_if, [], do_else) -> Printf.sprintf "If\n\t| %s\n\t| %s\n\t| %s\n"
                                    (string_of_expression cond)
                                    (string_of_expression do_if)
                                    (string_of_expression do_else)

  | If (cond, do_if, elif_list, do_else) -> Printf.sprintf "If\n\t| %s\n\t| %s\n\t| %s \n\t| %s\n"
                                            (string_of_expression cond)
                                            (string_of_expression do_if)
                                            (List.map string_of_expression elif_list |>
                                             List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "")
                                            (string_of_expression do_else)

  | Elif (cond, do_if) -> Printf.sprintf "Elif\n\t| %s\n\t| %s\n"
                          (string_of_expression cond)
                          (string_of_expression do_if)

  | Else do_else -> Printf.sprintf "Else\n\t| %s\n" @@ string_of_expression do_else

  | Type t -> Printf.sprintf "Type\n\t| %s\n" @@ string_of_type_expression t

  | IntLiteral n -> Printf.sprintf "IntLiteral\n\t| %d\n" n

  | RealLiteral x -> Printf.sprintf "RealLiteral\n\t| %f\n" x

  | StringLiteral s -> Printf.sprintf "StringLiteral\n\t| %s\n" s

  | AtomLiteral a -> Printf.sprintf "AtomLiteral\n\t| %s\n" a

  | BoolLiteral true -> "BoolLiteral\n\t| true\n"

  | BoolLiteral false -> "BoolLiteral\n\t| false\n"

  | TupleLiteral exp_list -> Printf.sprintf "TupleLiteral\n\t| %s\n"
                             (List.map string_of_expression exp_list |>
                              List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "")

  | ListLiteral exp_list -> Printf.sprintf "ListLiteral\n\t| %s\n"
                            (List.map string_of_expression exp_list |>
                             List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "")

  | MatrixLiteral exp_matrix -> Printf.sprintf "MatrixLiteral\n\t| %s\n"
                                (List.map (List.map string_of_expression) exp_matrix |>
                                 List.map (List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "") |>
                                 List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "")
