open Ast
open Symbols_table


let rec fill_constants (ast:Ast.expression) (st:symbols_table) : symbols_table =
  let rec fill_constants_lists lst st =
    match lst with
    | [] -> st
    | hd::tl -> fill_constants hd st |> fill_constants_lists tl
  in
  match ast with

  | Sequence (_, expressions) -> fill_constants_lists expressions st

  | Parentheses (_, e) -> fill_constants e st

  | Block (_, e) ->
    fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | BinOp (_, _, e1, e2) -> fill_constants e1 st |> fill_constants e2

  | UnOp (_, _, e) -> fill_constants e st

  | Lambda (_, args, t, e) -> fill_constants e st

  | FunctionCall (_, e, e_opt) ->
    let new_st = fill_constants e st in
    fill_constants e (new symbols_table (Some (ref new_st))) |> new_st#add_child; new_st

  | Annotation (_, n, t) ->
    begin match st#find_variable_in_scope n with
    | Some a -> st#update_entry a (VariableEntry (n, t, false)); st
    | None -> st#add_entry (VariableEntry (n, t, false)); st
    end

  | VariantInstance (i, v, c, e_opt) ->
    begin match e_opt with
    | Some e -> fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st
    | _ -> st
    end

  | Assignment (_, true, n, e) ->
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (n, t, init)) -> st#update_entry (VariableEntry (n, t, init)) (VariableEntry (n, t, true))
    | Some (VariantEntry _) -> ()
    | None -> st#add_entry (VariableEntry (n, Unknown, true))
    end;
    fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | If (_, cond, e, elif_list, opt_else) ->
    let cond_st = fill_constants cond (new symbols_table (Some (ref st))) in
    fill_constants e (new symbols_table (Some (ref cond_st))) |> cond_st#add_child;
    st#add_child cond_st;
    let st = fill_constants_lists elif_list st in
      begin match opt_else with
      | Some _else -> fill_constants _else st
      | None -> st
      end

  | Elif (_, cond, e) ->
    let cond_st = fill_constants cond (new symbols_table (Some (ref st))) in
    fill_constants e (new symbols_table (Some (ref cond_st))) |> cond_st#add_child;
    st#add_child cond_st;
    st

  | Else (_, e) ->
    fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | Match (_, m, match_list) ->
    let rec fill_constants_match lst st =
      match lst with
      | [] -> st
      | hd::tl ->
        fill_constants (snd hd) (new symbols_table (Some (ref st))) |> st#add_child; st
      in
    let match_st = fill_constants m (new symbols_table (Some (ref st))) in
    fill_constants_match match_list match_st |> st#add_child; st

  | _ -> st



let rec scope_check (ast:Ast.expression) (st:symbols_table) : (symbols_table, info * string) result =
  let rec scope_check_lists st = function
    | [] -> Ok st
    | hd::tl ->
      match scope_check hd st with
      | Ok st -> scope_check_lists st tl
      | e -> e
  in
  match ast with

  | Sequence (_, expressions) -> scope_check_lists st expressions

  | Parentheses (_, e) -> scope_check e st

  | Block (_, e) ->
    begin match scope_check e (new symbols_table (Some (ref st))) with
    | Ok new_st -> st#add_child new_st; Ok st
    | e -> e
    end

  | BinOp (_, _, e1, e2) ->
    begin match scope_check e1 st with
    | Ok st -> scope_check e2 st
    | e -> e
    end

  | UnOp (_, _, e) -> scope_check e st

  | Variable (i, n) ->
    begin match st#find_variable n with
    | Some v ->
      begin match v with
      | VariableEntry (n, _, false) -> Error (i, Printf.sprintf "The variable %s is not initialized." n)
      | _ -> Ok st
      end
    | None -> Error (i, Printf.sprintf "")
    end

  | Lambda (_, args, t, e) ->
    let new_st = (new symbols_table (Some (ref st))) in
    let arg_types =
      match t with
      | Tuple l -> l
      | t -> [t]
    in
    let rec add_args = function
      | [] -> ()
      | (n, t)::tl -> new_st#add_entry (VariableEntry (n, t, true)); add_args tl
    in
      List.combine args arg_types |> add_args;
      begin match scope_check e new_st with
      | Ok new_st -> st#add_child new_st; Ok st
      | e -> e
      end

  | FunctionCall (_, e, e_opt) ->
    begin match scope_check e st with
    | Ok st ->
      begin match e_opt with
      | Some e -> scope_check e st
      | None -> Ok st
      end
    | e -> e
    end

  | Annotation (_, n, t) ->
    begin match st#find_variable n with
    | Some _ -> Ok st
    | None -> st#add_entry (VariableEntry (n, t, false)); Ok st
    end

  | VariantDeclaration (_, v, constructors) -> st#add_entry (VariantEntry (v, constructors)); Ok st

  | VariantInstance (i, v, c, e_opt) ->
    begin match st#find_variant_constructor v c with
    | Some _ ->
      begin match e_opt with
      | Some e -> scope_check e st
      | None -> Ok st
      end
    | None -> Error (i, Printf.sprintf "Variant %s::%s not defined." v c)
    end

  | Assignment (_, false, n, _) ->
    begin match st#find_variable n with
    | Some (VariableEntry (n, t, init)) -> st#update_entry (VariableEntry (n, t, init)) (VariableEntry (n, t, true)); Ok st
    | Some (VariantEntry _) -> Ok st
    | None -> st#add_entry (VariableEntry (n, Unknown, true)); Ok st
    end

  | If (_, cond, e, elif_list, opt_else) ->
    begin match scope_check cond st with
    | Ok st ->
      begin match scope_check e st with
      | Ok st ->
        begin match scope_check_lists st elif_list  with
        | Ok st ->
          begin match opt_else with
          | Some e ->
            begin match scope_check e st with
            | Ok st -> Ok st
            | e -> e
            end
          | None -> Ok st
          end
        | e -> e
        end
      | e -> e
      end
    | e -> e
    end

  | Elif (_, cond, e) ->
    begin match scope_check cond st with
    | Ok st ->
      begin match scope_check e st with
      | Ok st -> Ok st
      | e -> e
      end
    | e -> e
    end

  | Else (_, e) ->
    begin match scope_check e st with
    | Ok st -> Ok st
    | e -> e
    end

  | Match (_, m, match_list) ->
    let rec scope_check_matchs st = function
      | [] -> Ok st
      | hd::tl ->
        let (pattern, e) = hd in
        match scope_check pattern st with
        | Ok st ->
          begin match scope_check e st with
          | Ok st -> scope_check_matchs st tl
          | e -> e
          end
        | e -> e
    in
    begin match scope_check m st with
    | Ok st -> scope_check_matchs st match_list
    | e -> e
    end

  | TupleLiteral (_, e_list) -> scope_check_lists st e_list

  | ListLiteral (_, e_list) -> scope_check_lists st e_list

  | VectorLiteral (_, e_list) -> scope_check_lists st e_list

  | MatrixLiteral (_, e_matrix) ->
    let rec scope_check_matrix st = function
      | [] -> Ok st
      | hd::tl ->
        match scope_check_lists st hd with
        | Ok st -> scope_check_matrix st tl
        | e -> e
    in scope_check_matrix st e_matrix

  | DictionaryLiteral (_, key_value_list) ->
    let rec scope_check_pairs st = function
      | [] -> Ok st
      | hd::tl ->
        let (k, v) = hd in
        match scope_check k st with
        | Ok st ->
          begin match scope_check v st with
          | Ok st -> scope_check_pairs st tl
          | e -> e
          end
        | e -> e
    in scope_check_pairs st key_value_list

  | _ -> Ok st
