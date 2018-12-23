type expression = Sequence of expression list
                | Parenthesis of expression
                | Block of expression
                | Lambda of string * type_expression * expression
                | FunctionCall of string * expression
                | Declaration of string * type_expression
                | Assignment of string * expression
                | If of expression * expression *
                        (expression * expression) list *
                        optional expression
                | Type of type_expression
                | IntLiteral of int
                | RealLiteral of float
                | StringLiteral of string
                | AtomLiteral of string
                | BoolLiteral of bool
                | TupleLiteral of expression list
                | ListLiteral of expression list
                | MatrixLiteral of expression list list

(*
let string_of_expression = function
  | Sequence exp_list -> Printf.sprintf "Sequence\n\t| %s" @@
                         List.map string_of_expression exp_list |>
                         List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) ""

  | Parenthesis e -> Printf.sprintf "Parenthesis\n\t| %s" @@ string_of_expression e

  | Block e -> Printf.sprintf "Block\n\t| %s" @@ string_of_expression e

  | Lambda v t e -> Printf.sprintf "Lambda(%s)\n\t| %s\n\t| %s"
                    (string_of_expression v)
                    (string_of_expression t)
                    (string_of_expression e)

  | FunctionCall name arg -> Printf.sprintf "FunctionCall(%s)\n\t| %s"
                             (string_of_expression name)
                             (string_of_expression arg)

  | Declaration name t -> Printf.sprintf "Declaration(%s)\n\t| %s"
                          (string_of_expression name)
                          (string_of_expression t)

  | Assignment name e -> Printf.sprintf "Assignment(%s)\n\t| %s"
                         (string_of_expression name)
                         (string_of_expression e)

  | If cond do_if [] None -> Printf.sprintf "If\n\t| %s\n\t| %s\n\t)"
                             (string_of_expression cond)
                             (string_of_expression do_if)

  | If cond do_if elif_list None -> Printf.sprintf "If\n\t| %s\n\t| %s\n\t| %s)"
                                    (string_of_expression cond)
                                    (string_of_expression do_if)
                                    (List.map (fun cond e -> Printf.sprintf "Elif\n\t| %s\n\t| %s"
                                                             (strig_of_expression cond)
                                                             (string_of_expression e))
                                              elif_list |>
                                     List.fold_left (fun s1 s2 -> s1 ^ "\n\t| " ^ s2) "")

  | If cond do_if [] Some(do_else) -> Printf.sprintf "If\n\t| %s\n\t| %s\n\t| Else\n\t| %s)"
  | If cond do_if elif_list Some(do_else) -> Printf.sprintf "If\n\t| %s\n\t| %s\n\t| Elif\n\t| %s \n\t| Else\n\t| %s)"
*)

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
