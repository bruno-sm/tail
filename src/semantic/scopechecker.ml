open Ast
open Symbols_table


let fill_predefined (st:symbols_table) : symbols_table =
  st#add_entry (FunctionEntry ("write", Tuple([String; WriteFile]), Void));
  st#add_entry (FunctionEntry ("write", String, Void));
  st#add_entry (FunctionEntry ("writeln", Tuple([String; WriteFile]), Void));
  st#add_entry (FunctionEntry ("writeln", String, Void));
  st#add_entry (FunctionEntry ("read", ReadFile, String));
  st#add_entry (FunctionEntry ("read", Void, String));
  st#add_entry (FunctionEntry ("readln", ReadFile, String));
  st#add_entry (FunctionEntry ("readln", Void, String));
  st#add_entry (VariableEntry ("stdin", ReadFile, true));
  st#add_entry (VariableEntry ("stdout", WriteFile, true));
  st#add_entry (VariableEntry ("stderr", WriteFile, true));
  st#add_entry (FunctionEntry ("to_string", Unknown, String));
  st#add_entry (FunctionEntry ("typeof·", Unknown, String));
  st#add_entry (FunctionEntry ("not·", Bool, Bool));
  st#add_entry (FunctionEntry ("-·", Int, Int));
  st#add_entry (FunctionEntry ("+·", Int, Int));
  st#add_entry (FunctionEntry ("-·", Real, Real));
  st#add_entry (FunctionEntry ("+·", Real, Real));
  st#add_entry (FunctionEntry ("-·", Rational, Rational));
  st#add_entry (FunctionEntry ("+·", Rational, Rational));
  st#add_entry (FunctionEntry ("-·", Complex, Complex));
  st#add_entry (FunctionEntry ("+·", Complex, Complex));
  st#add_entry (FunctionEntry ("·and·", Tuple([Bool; Bool]), Bool));
  st#add_entry (FunctionEntry ("·or·", Tuple([Bool; Bool]), Bool));
  st#add_entry (FunctionEntry ("·xor·", Tuple([Bool; Bool]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Bool; Bool]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Bool; Bool]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Int; Int]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Int; Int]), Bool));
  st#add_entry (FunctionEntry ("·<·", Tuple([Int; Int]), Bool));
  st#add_entry (FunctionEntry ("·<=·", Tuple([Int; Int]), Bool));
  st#add_entry (FunctionEntry ("·>·", Tuple([Int; Int]), Bool));
  st#add_entry (FunctionEntry ("·>=·", Tuple([Int; Int]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Real; Real]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Real; Real]), Bool));
  st#add_entry (FunctionEntry ("·<·", Tuple([Real; Real]), Bool));
  st#add_entry (FunctionEntry ("·<=·", Tuple([Real; Real]), Bool));
  st#add_entry (FunctionEntry ("·>·", Tuple([Real; Real]), Bool));
  st#add_entry (FunctionEntry ("·>=·", Tuple([Real; Real]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Rational; Rational]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Rational; Rational]), Bool));
  st#add_entry (FunctionEntry ("·<·", Tuple([Rational; Rational]), Bool));
  st#add_entry (FunctionEntry ("·<=·", Tuple([Rational; Rational]), Bool));
  st#add_entry (FunctionEntry ("·>·", Tuple([Rational; Rational]), Bool));
  st#add_entry (FunctionEntry ("·>=·", Tuple([Rational; Rational]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Complex; Complex]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Complex; Complex]), Bool));
  st#add_entry (FunctionEntry ("·<·", Tuple([Complex; Complex]), Bool));
  st#add_entry (FunctionEntry ("·<=·", Tuple([Complex; Complex]), Bool));
  st#add_entry (FunctionEntry ("·>·", Tuple([Complex; Complex]), Bool));
  st#add_entry (FunctionEntry ("·>=·", Tuple([Complex; Complex]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([String; String]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([String; String]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Atom; Atom]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Atom; Atom]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([List Unknown; List Unknown]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([List Unknown; List Unknown]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Vector Unknown; Vector Unknown]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Vector Unknown; Vector Unknown]), Bool));
  st#add_entry (FunctionEntry ("·=·", Tuple([Dictionary (Unknown, Unknown); Dictionary (Unknown, Unknown)]), Bool));
  st#add_entry (FunctionEntry ("·!=·", Tuple([Dictionary (Unknown, Unknown); Dictionary (Unknown, Unknown)]), Bool));
  st#add_entry (FunctionEntry ("·i", Int, Complex));
  st#add_entry (FunctionEntry ("·i", Real, Complex));
  st#add_entry (FunctionEntry ("·i", Rational, Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Int; Int]), Int));
  st#add_entry (FunctionEntry ("·+·", Tuple([Int; Real]), Real));
  st#add_entry (FunctionEntry ("·+·", Tuple([Real; Int]), Real));
  st#add_entry (FunctionEntry ("·+·", Tuple([Real; Real]), Real));
  st#add_entry (FunctionEntry ("·+·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·+·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·+·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·+·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·+·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·+·", Tuple([Int; Complex]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Real; Complex]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Rational; Complex]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Complex; Int]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Complex; Real]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Complex; Rational]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([Complex; Complex]), Complex));
  st#add_entry (FunctionEntry ("·+·", Tuple([String; String]), String));
  st#add_entry (FunctionEntry ("·-·", Tuple([Int; Int]), Int));
  st#add_entry (FunctionEntry ("·-·", Tuple([Int; Real]), Real));
  st#add_entry (FunctionEntry ("·-·", Tuple([Real; Int]), Real));
  st#add_entry (FunctionEntry ("·-·", Tuple([Real; Real]), Real));
  st#add_entry (FunctionEntry ("·-·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·-·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·-·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·-·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·-·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·-·", Tuple([Int; Complex]), Complex));
  st#add_entry (FunctionEntry ("·-·", Tuple([Real; Complex]), Complex));
  st#add_entry (FunctionEntry ("·-·", Tuple([Rational; Complex]), Complex));
  st#add_entry (FunctionEntry ("·-·", Tuple([Complex; Int]), Complex));
  st#add_entry (FunctionEntry ("·-·", Tuple([Complex; Real]), Complex));
  st#add_entry (FunctionEntry ("·-·", Tuple([Complex; Rational]), Complex));
  st#add_entry (FunctionEntry ("·-·", Tuple([Complex; Complex]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Int; Int]), Int));
  st#add_entry (FunctionEntry ("·*·", Tuple([Int; Real]), Real));
  st#add_entry (FunctionEntry ("·*·", Tuple([Real; Int]), Real));
  st#add_entry (FunctionEntry ("·*·", Tuple([Real; Real]), Real));
  st#add_entry (FunctionEntry ("·*·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·*·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·*·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·*·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·*·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·*·", Tuple([Int; Complex]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Real; Complex]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Rational; Complex]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Complex; Int]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Complex; Real]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Complex; Rational]), Complex));
  st#add_entry (FunctionEntry ("·*·", Tuple([Complex; Complex]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Int; Int]), Int));
  st#add_entry (FunctionEntry ("·/·", Tuple([Int; Real]), Real));
  st#add_entry (FunctionEntry ("·/·", Tuple([Real; Int]), Real));
  st#add_entry (FunctionEntry ("·/·", Tuple([Real; Real]), Real));
  st#add_entry (FunctionEntry ("·/·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·/·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·/·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·/·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·/·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·/·", Tuple([Int; Complex]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Real; Complex]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Rational; Complex]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Complex; Int]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Complex; Real]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Complex; Rational]), Complex));
  st#add_entry (FunctionEntry ("·/·", Tuple([Complex; Complex]), Complex));
  st#add_entry (FunctionEntry ("·//·", Tuple([Int; Int]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Int; Real]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Real; Int]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Real; Real]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·//·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·%·", Tuple([Int; Int]), Int));
  st#add_entry (FunctionEntry ("·%·", Tuple([Int; Real]), Real));
  st#add_entry (FunctionEntry ("·%·", Tuple([Real; Int]), Real));
  st#add_entry (FunctionEntry ("·%·", Tuple([Real; Real]), Real));
  st#add_entry (FunctionEntry ("·%·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·%·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·%·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·%·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·%·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·%·", Tuple([Int; Complex]), Complex));
  st#add_entry (FunctionEntry ("·%·", Tuple([Real; Complex]), Complex));
  st#add_entry (FunctionEntry ("·%·", Tuple([Rational; Complex]), Complex));
  st#add_entry (FunctionEntry ("·%·", Tuple([Complex; Int]), Complex));
  st#add_entry (FunctionEntry ("·%·", Tuple([Complex; Real]), Complex));
  st#add_entry (FunctionEntry ("·%·", Tuple([Complex; Rational]), Complex));
  st#add_entry (FunctionEntry ("·%·", Tuple([Complex; Complex]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Int; Int]), Int));
  st#add_entry (FunctionEntry ("·^·", Tuple([Int; Real]), Real));
  st#add_entry (FunctionEntry ("·^·", Tuple([Real; Int]), Real));
  st#add_entry (FunctionEntry ("·^·", Tuple([Real; Real]), Real));
  st#add_entry (FunctionEntry ("·^·", Tuple([Int; Rational]), Rational));
  st#add_entry (FunctionEntry ("·^·", Tuple([Real; Rational]), Rational));
  st#add_entry (FunctionEntry ("·^·", Tuple([Rational; Real]), Rational));
  st#add_entry (FunctionEntry ("·^·", Tuple([Rational; Int]), Rational));
  st#add_entry (FunctionEntry ("·^·", Tuple([Rational; Rational]), Rational));
  st#add_entry (FunctionEntry ("·^·", Tuple([Int; Complex]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Real; Complex]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Rational; Complex]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Complex; Int]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Complex; Real]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Complex; Rational]), Complex));
  st#add_entry (FunctionEntry ("·^·", Tuple([Complex; Complex]), Complex));
  st#add_entry (FunctionEntry ("·[·]", Tuple([Vector Unknown; Int]), Unknown));
  st#add_entry (FunctionEntry ("·[·]", Tuple([Matrix Unknown; Tuple([Int; Int])]), Unknown));
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

  | PrefixOp (_, _, e) -> fill_constants e st

  | PostfixOp (_, _, e) -> fill_constants e st

  | Lambda (_, args, t, e) -> fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | FunctionCall (_, e, e_opt) ->
    let new_st = fill_constants e st in
    begin match e_opt with
    | Some e -> fill_constants e (new symbols_table (Some (ref new_st))) |> new_st#add_child; new_st
    | _ -> new_st
    end

  | Annotation (_, n, t) ->
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (_, vt, false)) ->
      if vt = t then
        st
      else begin
        st#add_entry (VariableEntry (n, vt, false)); st
      end
    | _ -> st#add_entry (VariableEntry (n, t, false)); st
    end

  | VariantInstance (i, v, c, e_opt) ->
    begin match e_opt with
    | Some e -> fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st
    | _ -> st
    end

  | Assignment (_, n, e) ->
    fill_constants e (new symbols_table (Some (ref st))) |> st#add_child; st

  | Function (_, n, args, e) ->
    let e_st = (new symbols_table (Some (ref st))) in
    let rec add_args st = function
      | [] -> ()
      | (n, t)::tl -> st#add_entry (VariableEntry (n, t, true)); add_args st tl
    in
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (_, vt, false)) ->
      begin match vt with
      | Arrow (ts, td) -> let args_types =
                            match ts with
                            | Tuple l -> l
                            | t -> [t]
                          in
                          if List.length args = List.length args_types then begin
                            st#add_entry (FunctionEntry (n, ts, td));
                            st#remove_entry_in_scope (VariableEntry (n, vt, false));
                            add_args e_st (List.combine args args_types);
                            fill_constants e e_st |> st#add_child;
                            st
                          end else
                            st
      | _ -> st#add_entry (FunctionEntry (n, Unknown, Unknown));
             fill_constants e (new symbols_table (Some (ref st))) |> st#add_child;
             st
      end
    | _ -> st#add_entry (FunctionEntry (n, Unknown, Unknown));
           fill_constants e (new symbols_table (Some (ref st))) |> st#add_child;
           st
    end

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

  | PrefixOp (_, _, e) -> scope_check e st

  | PostfixOp (_, _, e) -> scope_check e st

  | Variable (i, n) ->
    begin match st#find_variable n with
    | Some v ->
      begin match v with
      | VariableEntry (n, _, false) -> Error (i, Printf.sprintf "The variable %s is not initialized." n)
      | _ -> Ok st
      end
    | None -> Error (i, Printf.sprintf "%s doesn't exist." n)
    end

  | Lambda (i, args, _, e) ->
    let new_st = st#next_child in
    let rec add_args = function
      | [] -> ()
      | hd::tl -> new_st#add_entry (VariableEntry (hd, Unknown, true)); add_args tl
    in
      add_args args;
      begin match scope_check e new_st with
      | Ok new_st -> st#update_child new_st; Ok st
      | e -> e
      end

  | FunctionCall (i, e, e_opt) ->
    begin match e with
    | Variable (_, n) ->
      begin match st#find_function_by_name n with
      | Some (FunctionEntry (n, ts, td)) ->
        begin match e_opt with
        | Some e ->
          begin match scope_check e st#next_child with
          | Ok new_st -> st#update_child new_st; Ok st
          | e -> e
          end
        | None -> Ok st
        end
      | _ -> Error (i, Printf.sprintf "Function %s is not defined." n)
      end
    | _ ->
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
    end

  | Annotation (i, n, t) ->
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (n, t, init)) -> st#update_entry (VariableEntry (n, t, init)) (VariableEntry (n, t, false)); Ok st
    | _ -> st#add_entry (VariableEntry (n, t, false)); Ok st
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

  | Assignment (i, n, e) ->
    let check_e st =
      begin match scope_check e st#next_child with
      | Ok new_st ->  Ok st
      | e -> e
      end
    in
    begin match st#find_variable_in_scope n with
    | Some (VariableEntry (n, t, init)) -> st#update_entry (VariableEntry (n, t, init)) (VariableEntry (n, t, true)); check_e st
    | _ -> st#add_entry (VariableEntry (n, Unknown, true)); check_e st
    end

  | Function (_, n, args, e) ->
    let new_st = st#next_child in
    begin match scope_check e new_st with
    | Ok new_st -> st#update_child new_st; Ok st
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
