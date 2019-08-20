open Ast
open Symbols_table


let fill_predefined (st:symbols_table) : symbols_table =
  st#add_entry (VariableEntry ("write", Arrow(WriteFile, Void), true, true));
  st#add_entry (VariableEntry ("write", Arrow(Void, Void), true, true));
  st#add_entry (VariableEntry ("writeln", Arrow(WriteFile, Void), true, true));
  st#add_entry (VariableEntry ("writeln", Arrow(Void, Void), true, true));
  st#add_entry (VariableEntry ("read", Arrow(ReadFile, String), true, true));
  st#add_entry (VariableEntry ("read", Arrow(Void, String), true, true));
  st#add_entry (VariableEntry ("readln", Arrow(ReadFile, String), true, true));
  st#add_entry (VariableEntry ("readln", Arrow(Void, String), true, true));
  st#add_entry (VariableEntry ("stdin", ReadFile, true, true));
  st#add_entry (VariableEntry ("stdout", WriteFile, true, true));
  st#add_entry (VariableEntry ("stderr", WriteFile, true, true));
  st#add_entry (VariableEntry ("to_string", Arrow(Unknown, Unknown), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Int; Int]), Int), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Int; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Real; Int]), Real), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Real; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Int; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Real; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Rational; Real]), Rational), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Rational; Int]), Rational), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Rational; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Int; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Real; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Rational; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Complex; Int]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Complex; Real]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Complex; Rational]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([Complex; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·+·", Arrow(Tuple([String; String]), String), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Int; Int]), Int), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Int; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Real; Int]), Real), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Real; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Int; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Real; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Rational; Real]), Rational), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Rational; Int]), Rational), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Rational; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Int; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Real; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Rational; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Complex; Int]), Complex), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Complex; Real]), Complex), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Complex; Rational]), Complex), true, true));
  st#add_entry (VariableEntry ("·-·", Arrow(Tuple([Complex; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Int; Int]), Int), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Int; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Real; Int]), Real), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Real; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Int; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Real; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Rational; Real]), Rational), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Rational; Int]), Rational), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Rational; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Int; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Real; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Rational; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Complex; Int]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Complex; Real]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Complex; Rational]), Complex), true, true));
  st#add_entry (VariableEntry ("·*·", Arrow(Tuple([Complex; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Int; Int]), Int), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Int; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Real; Int]), Real), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Real; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Int; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Real; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Rational; Real]), Rational), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Rational; Int]), Rational), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Rational; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Int; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Real; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Rational; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Complex; Int]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Complex; Real]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Complex; Rational]), Complex), true, true));
  st#add_entry (VariableEntry ("·/·", Arrow(Tuple([Complex; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Int; Int]), Int), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Int; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Real; Int]), Real), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Real; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Int; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Real; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Rational; Real]), Rational), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Rational; Int]), Rational), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Rational; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Int; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Real; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Rational; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Complex; Int]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Complex; Real]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Complex; Rational]), Complex), true, true));
  st#add_entry (VariableEntry ("·%·", Arrow(Tuple([Complex; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Int; Int]), Int), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Int; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Real; Int]), Real), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Real; Real]), Real), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Int; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Real; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Rational; Real]), Rational), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Rational; Int]), Rational), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Rational; Rational]), Rational), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Int; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Real; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Rational; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Complex; Int]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Complex; Real]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Complex; Rational]), Complex), true, true));
  st#add_entry (VariableEntry ("·^·", Arrow(Tuple([Complex; Complex]), Complex), true, true));
  st#add_entry (VariableEntry ("·[·]", Arrow(Tuple([Vector Unknown; Int]), Unknown), true, true));
  st#add_entry (VariableEntry ("·[·]", Arrow(Tuple([Matrix Unknown; Tuple([Int; Int])]), Unknown), true, true));
  st


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

  | Lambda (_, args, t, e) -> fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | FunctionCall (_, e, e_opt) ->
    let new_st = fill_constants e st in
    begin match e_opt with
    | Some e -> fill_constants e (new symbols_table (Some (ref new_st))) |> new_st#add_child; new_st
    | _ -> new_st
    end

  | Annotation (_, n, t) ->
    begin match st#find_variable_in_scope n with
    | Some a -> st#update_entry a (VariableEntry (n, t, false, false)); st
    | None -> st#add_entry (VariableEntry (n, t, false, false)); st
    end

  | VariantInstance (i, v, c, e_opt) ->
    begin match e_opt with
    | Some e -> fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st
    | _ -> st
    end

  | Assignment (_, true, n, e) ->
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (n, t, init, const)) -> st#update_entry (VariableEntry (n, t, init, const)) (VariableEntry (n, t, true, true))
    | Some (VariantEntry _) -> ()
    | None -> st#add_entry (VariableEntry (n, Unknown, true, true))
    end;
    fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | Assignment (_, false, n, e) ->
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
        fill_constants (snd hd) (new symbols_table (Some (ref st))) |> st#add_child;
        fill_constants_match tl st
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
  (*st#to_string "" |> print_endline;*)
  match ast with

  | Sequence (_, expressions) -> scope_check_lists st expressions

  | Parentheses (_, e) -> scope_check e st

  | Block (_, e) ->
    begin match scope_check e st#next_child with
    | Ok new_st -> Ok st
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
      | VariableEntry (n, _, false, _) -> Error (i, Printf.sprintf "The variable %s is not initialized." n)
      | _ -> Ok st
      end
    | None -> Error (i, Printf.sprintf "%s doesn't exist." n)
    end

  | Lambda (i, args, _, e) ->
    let new_st = st#next_child in
    let rec add_args = function
      | [] -> ()
      | hd::tl -> new_st#add_entry (VariableEntry (hd, Unknown, true, true)); add_args tl
    in
      add_args args;
      begin match scope_check e new_st with
      | Ok new_st -> st#update_child new_st; Ok st
      | e -> e
      end

  | FunctionCall (_, e, e_opt) ->
    begin match scope_check e st with
    | Ok st ->
      begin match e_opt with
      | Some e ->
        begin match scope_check e st#next_child with
        | Ok new_st -> st#update_child new_st; Ok st
        | e -> e
        end
      | None -> Ok st
      end
    | e -> e
    end

  | Annotation (i, n, t) ->
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (n, t, init, false)) -> st#update_entry (VariableEntry (n, t, init, false)) (VariableEntry (n, t, false, false)); Ok st
    | Some (VariableEntry (n, new_t, init, true)) -> if t = new_t then Ok st else Error (i, Printf.sprintf "%s can not be retyped." n)
    | None -> st#add_entry (VariableEntry (n, t, false, false)); Ok st
    end

  | VariantDeclaration (_, v, constructors) -> st#add_entry (VariantEntry (v, constructors)); Ok st

  | VariantInstance (i, v, c, e_opt) ->
    begin match st#find_variant_constructor v c with
    | Some _ ->
      begin match e_opt with
      | Some e ->
        begin match scope_check e st#next_child with
        | Ok new_st -> st#update_child new_st; Ok st
        | e -> e
        end
      | None -> Ok st
      end
    | None -> Error (i, Printf.sprintf "Variant %s::%s not defined." v c)
    end

  | Assignment (i, false, n, e) ->
    let check_e st =
      begin match scope_check e st#next_child with
      | Ok new_st ->  Ok st
      | e -> e
      end
    in
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (n, t, init, false)) -> st#update_entry (VariableEntry (n, t, init, false)) (VariableEntry (n, t, true, false)); check_e st
    | Some (VariableEntry _) -> Error (i, Printf.sprintf "%s can not be redefined." n)
    | Some (VariantEntry _) -> check_e st
    | None -> st#add_entry (VariableEntry (n, Unknown, true, false)); check_e st
    end

  | Assignment (i, true, n, e) ->
    begin match scope_check e st#next_child with
    | Ok new_st -> Ok st
    | e -> e
    end

  | If (_, cond, e, elif_list, opt_else) ->
    begin match scope_check cond st#next_child with
    | Ok cond_st ->
      begin match scope_check e cond_st#next_child with
      | Ok e_st ->
        cond_st#update_child e_st;
        st#update_child cond_st;
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
    begin match scope_check cond st#next_child with
    | Ok cond_st ->
      begin match scope_check e cond_st#next_child with
      | Ok e_st -> cond_st#update_child e_st; st#update_child cond_st; Ok st
      | e -> e
      end
    | e -> e
    end

  | Else (_, e) ->
    begin match scope_check e st#next_child with
    | Ok e_st -> st#update_child e_st; Ok st
    | e -> e
    end

  | Match (_, m, match_list) ->
    let rec scope_check_matchs st = function
      | [] -> Ok st
      | hd::tl ->
        let (pattern, e) = hd in
        match scope_check pattern st with
        | Ok st ->
          begin match scope_check e st#next_child with
          | Ok e_st -> st#update_child e_st; scope_check_matchs st tl
          | e -> e
          end
        | e -> e
    in
    begin match scope_check m st#next_child with
    | Ok m_st ->
      begin match scope_check_matchs m_st match_list with
      | Ok new_st -> st#update_child new_st; Ok st
      | e -> e
      end
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
