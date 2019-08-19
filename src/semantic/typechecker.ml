open Ast

(*
(* Checks type consistency and decorates the AST with the expression types *)
let rec type_check (e : expression) (st : symbols_table) : (info * expression, info * string) result =
  match e with
  | Sequence (i, expressions) ->
    let rec sequence_type_check expressions checked_expressions _type =
      match expressions with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=_type} in
              Ok (i, Sequence (i, checked_expressions))
      | e::l -> match type_check e with
                | Ok (i, e) -> sequence_type_check l (e::checked_expressions) i._type
                | Error (i, s) -> Error (i, s)
    in
    sequence_type_check expressions [] (Some (Void i))

  | Parentheses (i, e) ->
    match type_check e with
    | Ok (ei, e) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=ei._type} in
                    Ok (i, Parentheses (i, e))
    | Error (i, s) -> Error (i, s)

  | Block (i, e) ->
    match type_check e with
    | Ok (ei, e) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=ei._type} in
                      Ok (i, Block (i, e))
    | Error (i, s) -> Error (i, s)

  | BinOp (i, op, (e1i, e1), (e2i, e2)) ->
    let arg_type = Tuple(i, e1i._type, e2i._type) in
    let op_name = operator_name op in
    match st#get_function_restype(op_name, arg_type)) with
      None -> let i = {start_pos=e1i.start_pos; end_pos=e2i.end_pos; _type=None} in
              Error (i, Printf.sprintf "Operator %s is not defined for type %s."
                                       op_name (string_of_type_expression arg_type))
      Some res_type -> let i = {start_pos=e1i.start_pos; end_pos=e2i.end_pos; _type=res_type} in
                       Ok (i, BinOp (i, op, e1, e2))
*)
