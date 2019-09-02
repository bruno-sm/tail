open Ast
open Symbols_table


let rec max_extrema (t : type_expression) : type_expression =
  match t with
  | Unknown -> Universe
  | Arrow (s, d) -> Arrow (min_extrema s, max_extrema d)
  | Tuple l -> Tuple (List.map max_extrema l)
  | List t -> List (max_extrema t)
  | Vector t -> Vector (max_extrema t)
  | Matrix t -> Matrix (max_extrema t)
  | Dictionary (k, v) -> Dictionary (max_extrema k, max_extrema v)
  | Union (t1, t2) -> Union (max_extrema t1, max_extrema t2)
  | Intersection (t1, t2) -> Intersection (max_extrema t1, max_extrema t2)
  | Complement t -> Complement (max_extrema t)
  | ParenthesesType t -> ParenthesesType (max_extrema t)
  | t -> t

and min_extrema (t : type_expression) : type_expression =
  match t with
  | Unknown -> Void
  | Arrow (s, d) -> Arrow (max_extrema s, min_extrema d)
  | Tuple l -> Tuple (List.map min_extrema l)
  | List t -> List (min_extrema t)
  | Vector t -> Vector (min_extrema t)
  | Matrix t -> Matrix (min_extrema t)
  | Dictionary (k, v) -> Dictionary (min_extrema k, min_extrema v)
  | Union (t1, t2) -> Union (min_extrema t1, min_extrema t2)
  | Intersection (t1, t2) -> Intersection (min_extrema t1, min_extrema t2)
  | Complement t -> Complement (min_extrema t)
  | ParenthesesType t -> ParenthesesType (min_extrema t)
  | t -> t


let rec (<=) (t1 : type_expression) (t2 : type_expression) : bool =
  match (t1, t2) with
  | (_, Universe) -> true
  | (Int, Rational) -> true
  | (Int, Real) -> true
  | (Int, Complex) -> true
  | (Rational, Real) -> true
  | (Rational, Complex) -> true
  | (Real, Complex) -> true
  | (SpecificAtom _, Atom) -> true
  | (WriteFile, File) -> true
  | (ReadFile, File) -> true
  | (Arrow (t1, t2), Arrow (s1, s2)) -> s1 <= t1 && t2 <= s1
  | (Tuple l1, Tuple l2) -> List.fold_left2 (fun x t1 t2 -> x && t1 <= t2) true l1 l2
  | (Union(t11, t12), t2) -> t11 <= t2 && t12 <= t2
  | (t1, Union(t21, t22)) -> t1 <= t21 || t1 <= t22
  | (Intersection(Union(s1, s2), Union(t1, t2)), t) -> (not (s1 <= Union(t1, t2)) || s1 <= t) &&
                                                       (not (s2 <= Union(t1, t2)) || s2 <= t) &&
                                                       (not (t1 <= Union(s1, s2)) || t1 <= t) &&
                                                       (not (t2 <= Union(s1, s2)) || t2 <= t)
  | (Intersection(t11, t12), t2) -> (not (t11 <= t12) || t11 <= t2) &&
                                    (not (t12 <= t11) || t11 <= t2)
  | (t1, Intersection(t21, t22)) -> t1 <= t21 && t1 <= t22
  | (Complement (Union (t11, t12)), t21) -> Intersection(Complement(t11), Complement(t12)) <= t21
  | (Complement t11, Union(t21, t22)) -> not (t11 <= t21 || t11 <= t22)
  | (Complement (Complement t11), t2) -> t11 <= t2
  | (t1, Complement t21) -> Intersection(t1, t21) <= Void
  | (ParenthesesType t11, t2) -> t11 <= t2
  | (Void, _) -> true
  | (t1, t2) -> t1 = t2

and (<=~) (t1 : type_expression) (t2 : type_expression) : bool =
  min_extrema t1 <= max_extrema t2

and (</=~) (t1 : type_expression) (t2 : type_expression) : bool =
  not (max_extrema t1 <= min_extrema t2)


let rec aplicative_concretization_plus (t:type_expression) : type_expression list list =
  match t with
  | Void -> []
  | Unknown -> [[Arrow (Unknown, Unknown)]]
  | Arrow (s, t) -> [[Arrow (s, t)]]
  | Complement t -> aplicative_concretization_minus t
  | Union (t1, t2) -> List.append (aplicative_concretization_plus t1)
                                  (aplicative_concretization_plus t2)
  | Intersection (t1, t2) -> List.map2 (fun l1 l2 -> List.append l1 l2)
                                       (aplicative_concretization_plus t1)
                                       (aplicative_concretization_plus t2)
  | _ -> [[]]

and aplicative_concretization_minus (t:type_expression) : type_expression list list =
  match t with
  | Universe -> []
  | Complement t -> aplicative_concretization_plus t
  | Intersection (t1, t2) -> List.append (aplicative_concretization_minus t1)
                                         (aplicative_concretization_minus t2)
  | Union (t1, t2) -> List.map2 (fun l1 l2 -> List.append l1 l2)
                                (aplicative_concretization_minus t1)
                                (aplicative_concretization_minus t2)
  | _ -> [[]]


let rec simplify (t:type_expression) : type_expression =
  match t with
  | Union (Void, t2) -> simplify t2
  | Union (t1, Void) -> simplify t1
  | Union (t1, t2) ->
    let s1 = simplify t1 in
    let s2 = simplify t2 in
    if simplify s1 <= s2 then s2 else begin if s2 <= s1 then s1 else Union (s1, s2) end
  | Intersection (Universe, t2) -> simplify t2
  | Intersection (t1, Universe) -> simplify t1
  | Intersection (t1, t2) ->
    let s1 = simplify t1 in
    let s2 = simplify t2 in
    if simplify s1 <= s2 then s1 else begin if s2 <= s1 then s2 else Intersection (s1, s2) end
  | Complement (Complement t) -> simplify t
  | _ -> t


(* Operation  ⋃_{t € l} f(t) *)
let fold_union f l = List.fold_left (fun x t -> Union (x, f(t))) Void l

(* Operation ⋂_{t € l} f(t) *)
let fold_intersection f l = List.fold_left (fun x t -> Intersection (x, f(t))) Universe l


let dom (t:type_expression) : type_expression =
  fold_intersection
  begin fun s ->
    fold_union
    begin fun t ->
      match t with
      | Arrow(s1, s2) -> max_extrema s1
      | _ -> Void
    end
    s
  end
  (aplicative_concretization_plus t)
  |> simplify


let (@~) (t1:type_expression) (t2:type_expression) : type_expression =
  let setdiff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
  let rec powerset = function
    | [] -> [[]]
    | x :: xs ->
       let ps = powerset xs in
       ps @ List.map (fun ss -> x :: ss) ps
  in
  fold_union
  begin fun s ->
    let q_list = setdiff (powerset s) [s] in
    let cond1 q =
      t2 </=~ (fold_union
                begin fun t ->
                match t with
                | Arrow(s, t) -> s
                | _ -> Void
                end
                q)
    in
    let cond2 q =
      Intersection(max_extrema t2,
                   fold_union
                     begin fun t ->
                     match t with
                     | Arrow(s, t) -> max_extrema s
                     | _ -> Void
                     end
                     (setdiff s q))
      </=~ Void
    in
    let q_list = (List.filter (fun q -> cond1 q && cond2 q) q_list) in
    fold_union
    begin fun q ->
      fold_intersection
      begin fun t ->
        match t with
        | Arrow(s, t) -> t
        | _ -> Void
      end
      (setdiff s q)
    end
    q_list
  end
  (aplicative_concretization_plus t1)
  |> simplify


(* Checks type consistency and decorates the AST with the expression types *)
let rec type_check (e : expression) (st : symbols_table) : (info * expression, info * string) result =
  match e with
  | Sequence (i, expressions) ->
    let rec sequence_type_check expressions checked_expressions _type =
      begin match expressions with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=_type} in
              Ok (i, Sequence (i, (List.rev checked_expressions)))
      | e::l -> begin match type_check e st with
                | Ok (i, e) -> sequence_type_check l (e::checked_expressions) i._type
                | Error (i, s) -> Error (i, s)
                end
      end
    in
    sequence_type_check expressions [] Void

  | Parentheses (i, e) ->
    begin match type_check e st with
    | Ok (ei, e) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=ei._type} in
                    Ok (i, Parentheses (i, e))
    | Error (i, s) -> Error (i, s)
    end

  | Block (i, e) ->
    begin match type_check e st#next_child with
    | Ok (ei, e) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=ei._type} in
                      Ok (i, Block (i, e))
    | Error (i, s) -> Error (i, s)
    end

  | BinOp (_, op, e1, e2) ->
    begin match (type_check e1 st, type_check e2 st) with
    | (Ok (e1i, e1), Ok (e2i, e2)) ->
      let arg_type = Tuple([e1i._type; e2i._type]) in
      let op_name = "·" ^ (operator_name op) ^ "·" in
      begin match st#get_function_restype op_name arg_type with
      |None -> let i = {start_pos=e1i.start_pos; end_pos=e2i.end_pos; _type=Unknown} in
               Error (i, Printf.sprintf "Operator %s is not defined for type %s."
                                         op_name (string_of_type_expression arg_type))
      |Some res_type -> let i = {start_pos=e1i.start_pos; end_pos=e2i.end_pos; _type=res_type} in
                        Ok (i, BinOp (i, op, e1, e2))
      end
    | (Error (i, s), _) -> Error (i, s)
    | (_, Error (i, s)) -> Error (i, s)
    end

  | PrefixOp (i, op, e) ->
    begin match type_check e st with
    | Ok (ei, e) ->
      let arg_type = ei._type in
      let op_name = (operator_name op) ^ "·" in
      begin match st#get_function_restype op_name arg_type with
      |None -> let i = {start_pos=i.start_pos; end_pos=ei.end_pos; _type=Unknown} in
               Error (i, Printf.sprintf "Operator %s is not defined for type %s."
                                         op_name (string_of_type_expression arg_type))
      |Some res_type -> let i = {start_pos=i.start_pos; end_pos=ei.end_pos; _type=res_type} in
                        Ok (i, PrefixOp (i, op, e))
      end
    | Error (i, s) -> Error (i, s)
    end

  | PostfixOp (i, op, e) ->
    begin match type_check e st with
    | Ok (ei, e) ->
      let arg_type = ei._type in
      let op_name = "·" ^ (operator_name op) in
      begin match st#get_function_restype op_name arg_type with
      |None -> let i = {start_pos=i.start_pos; end_pos=ei.end_pos; _type=Unknown} in
               Error (i, Printf.sprintf "Operator %s is not defined for type %s."
                                         op_name (string_of_type_expression arg_type))
      |Some res_type -> let i = {start_pos=i.start_pos; end_pos=ei.end_pos; _type=res_type} in
                        Ok (i, PrefixOp (i, op, e))
      end
    | Error (i, s) -> Error (i, s)
    end

  | Variable (i, v) ->
    begin match st#find_variable v with
    | Some (VariableEntry (n, t, _)) ->
      let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=t} in
      Ok (i, Variable (i, v))
    | _ -> Error (i, Printf.sprintf "%s doesn't exist." v)
    end

  | Function (i, n, args, e) -> type_check e st#next_child

  | Lambda (i, args, t, e) ->
    begin match t with
    | Arrow (s, d) ->
      begin match type_check e st#next_child with
      | Ok (ie, e) ->
        if d = ie._type || d = Unknown then
          let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=Arrow(s, ie._type)} in
          Ok (i, Lambda (i, args, t, e))
        else
          Error (i, Printf.sprintf "This function is said to return %s type, but is returning %s type."
                                   (string_of_type_expression d)
                                   (string_of_type_expression ie._type))
      | Error (i, msg) -> Error (i, msg)
      end
    | _ -> Error (i, Printf.sprintf "A function must have an Arrow type instead of %s."
                                    (string_of_type_expression t))
    end

  | FunctionCall (i, f, opt_arg) ->
    begin match f with
    | Variable (i_v, v) ->
      begin match opt_arg with
      | Some arg ->
        begin match type_check arg st with
        | Ok (i_arg, arg) ->
          begin match st#get_function_restype v i_arg._type with
          | None -> Error (i_v, Printf.sprintf "Function %s is not defined for type %s."
                                               v (string_of_type_expression i_arg._type))
          | Some res_type -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=res_type} in
                             Ok (i, FunctionCall(i, f, Some arg))
          end
        | Error (i, msg) -> Error (i, msg)
        end
      | None ->
        begin match st#get_function_restype v Void with
        | None -> Error (i_v, Printf.sprintf "Function %s is not defined for type Void." v)
        | Some res_type -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=res_type} in
                           Ok (i, FunctionCall(i, f, None))
        end
      end
    | _ ->
      begin match type_check f st with
      | Ok (i_f, f) ->
        begin match i_f._type with
        | Arrow (s, d) ->
          begin match opt_arg with
          | Some arg ->
            begin match type_check arg st with
            | Ok (i_arg, arg) ->
              if i_arg._type = s || s = Unknown then
                let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=d} in
                Ok (i, FunctionCall(i, f, Some arg))
              else
                Error (i, Printf.sprintf "This function admits %s type as argument, but it's being applied to %s type."
                                         (string_of_type_expression s)
                                         (string_of_type_expression i_arg._type))
            | Error (i, msg) -> Error (i, msg)
            end
          | None ->
            let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=d} in
            Ok (i, FunctionCall(i, f, None))
          end
        | _ -> Error (i, Printf.sprintf "Function application only can be performed over Arrow types, but this expression have type %s."
                         (string_of_type_expression i_f._type))
        end
      | Error (i, msg) -> Error (i, msg)
      end
    end


  | Annotation (i, n, t) ->
    let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=t} in
    Ok (i, Annotation(i, n, t))


  | VariantDeclaration (i, n, c) ->
    let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=Void} in
    Ok (i, VariantDeclaration(i, n, c))


  | VariantInstance (i, v, c, opt_arg) ->
    begin match opt_arg with
    | Some arg ->
      begin match type_check arg st#next_child with
      | Ok (i_arg, arg) ->
        begin match st#find_variant_constructor v c with
        | Some (VariantEntry (v, c_list)) ->
          let constructor = List.find (fun x -> x.name = c) c_list in
          if constructor.argument_type = Unknown || constructor.argument_type = i_arg._type then
            let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(VariantConstructor (v, c))} in
            Ok (i, VariantInstance(i, v, c, Some arg))
          else
            Error (i, Printf.sprintf "The constructor %s::%s expects a type %s, but it's being applied to a type %s."
                                     v c (string_of_type_expression constructor.argument_type)
                                     (string_of_type_expression i_arg._type))
        | _ -> Error (i, Printf.sprintf "The constructor %s::%s doesn't exist." v c)
        end
      | Error (i, msg) -> Error (i, msg)
      end
    | None ->
        begin match st#find_variant_constructor v c with
        | Some (VariantEntry (v, c_list)) ->
          let constructor = List.find (fun x -> x.name = c) c_list in
          if constructor.argument_type = Void then
            let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(Variant v)} in
            Ok (i, VariantInstance(i, v, c, None))
          else
            Error (i, Printf.sprintf "The constructor %s::%s expects a type %s, but it's not being applied to any arguments."
                                     v c (string_of_type_expression constructor.argument_type))
        | _ -> Error (i, Printf.sprintf "The constructor %s::%s doesn't exist." v c)
        end
    end


  | VariantProjection(i, e, p) ->
    begin match type_check e st with
    | Ok (i_e, e) ->
      begin match i_e._type with
      | VariantConstructor (v, c) ->
        begin match st#find_variant_constructor v c with
        | Some (VariantEntry (v, c_list)) ->
            let constructor = List.find (fun x -> x.name = c) c_list in
            let argument_types =
              match constructor.argument_type with
              | Tuple l -> l
              | t -> [t]
            in
            begin match List.combine constructor.arguments argument_types |>
                        List.find_opt (fun x -> fst x = p) with
            | Some (p, p_type) ->
              let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=p_type} in
              Ok (i, VariantProjection(i, e, p))
            | None -> Error (i, Printf.sprintf "The constructor %s::%s doesn't have the attribute %s." v c p)
            end
        | _ -> Error (i, Printf.sprintf "The constructor %s::%s doesn't exist." v c)
        end
      | _ -> Error (i_e, Printf.sprintf "A projection only can be performed over a variant, but this expression is of type %s."
                                        (string_of_type_expression i_e._type))
      end
    | Error (i, msg) -> Error (i, msg)
    end


  | Assignment (i, n, e) ->
    begin match type_check e st#next_child with
    | Ok (i_e, e) ->
      begin match st#find_variable_in_scope n with
      | Some (VariableEntry (n, t, _)) ->
        let e_type =
          match i_e._type with
          | Arrow (Unknown, ed) ->
            begin match t with
            | Arrow (ts, td) ->
              if td = ed then Arrow (ts, ed) else i_e._type
            | _ -> i_e._type
            end
          | _ -> i_e._type
        in
        if t = Unknown || t = e_type then
          let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=e_type} in
          Ok (i, Assignment (i, n, e))
        else
          Error (i, Printf.sprintf "%s is said to be of type %s, but it's being assigned to a type %s."
                                   n (string_of_type_expression t)
                                   (string_of_type_expression i_e._type))
      | _ -> Error (i, Printf.sprintf "%s is not in the symbols table, this shouldn't happen." n)
      end
    | Error (i, msg) -> Error (i, msg)
    end


  | If (i, cond, e, elif_list, else_opt) ->
    let rec type_check_elif_list elif_list st i_list elif_list =
      match elif_list with
      | [] -> Ok (i_list, elif_list)
      | hd::tl ->
        begin match type_check hd st with
        | Ok (i_hd, hd) -> type_check_elif_list tl st (i_hd::i_list) (hd::elif_list)
        | Error (i, msg) -> Error (i, msg)
        end
    in
    let cond_st = st#next_child in
    begin match type_check cond cond_st with
    | Ok (i_cond, cond) ->
      begin match i_cond._type with
      | Bool ->
        begin match type_check e cond_st#next_child with
        | Ok (i_e, e) ->
          begin match type_check_elif_list elif_list st [] [] with
          | Ok (i_elif_list, elif_list) ->
            begin match else_opt with
            | Some _else ->
              begin match type_check _else st with
              | Ok (i_else, _else) ->
                let type_list = List.append (List.map (fun x -> x._type) i_elif_list) [i_else._type] in
                let union_types = List.fold_left (fun t1 t2 -> Union (t1, t2)) i_e._type type_list in
                let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=union_types} in
                        Ok (i, If(i, cond, e, elif_list, else_opt))
              | Error (i, msg) -> Error (i, msg)
              end
            | None ->
              let type_list = List.map (fun x -> x._type) i_elif_list in
              let union_types = List.fold_left (fun t1 t2 -> Union (t1, t2)) i_e._type type_list in
              let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=union_types} in
                      Ok (i, If(i, cond, e, elif_list, None))
            end
          | Error (i, msg) -> Error (i, msg)
          end
        | Error (i, msg) -> Error (i, msg)
        end
      | _ -> Error (i_cond, Printf.sprintf "An if condition must be of type Bool, but it's of type %s."
                                           (string_of_type_expression i_cond._type))
      end
    | Error (i, msg) -> Error (i, msg)
    end


  | Elif (i, cond, e) ->
    let cond_st = st#next_child in
    begin match type_check cond cond_st with
    | Ok (i_cond, cond) ->
      begin match i_cond._type with
      | Bool ->
        begin match type_check e cond_st#next_child with
        | Ok (i_e, e) ->
          let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=i_e._type} in
                  Ok (i, Elif(i, cond, e))
        | Error (i, msg) -> Error (i, msg)
        end
      | _ -> Error (i_cond, Printf.sprintf "An elif condition must be of type Bool, but it's of type %s."
                                           (string_of_type_expression i_cond._type))
      end
    | Error (i, msg) -> Error (i, msg)
    end


  | Else (i, e) ->
    begin match type_check e st#next_child with
    | Ok (i_e, e) ->
      let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=i_e._type} in
              Ok (i, Else(i, e))
    | Error (i, e) -> Error (i, e)
    end


  | Match (i, m, matches) ->
    let rec type_check_matches matches matches_i checked_matches =
      match matches with
      | [] -> Ok (List.rev matches_i, List.rev checked_matches)
      | (p, e)::tl ->
        begin match p with
        | VariantDecomposition (i, v, c, names) ->
          begin match type_check e st with
          | Ok (i_e, e) -> type_check_matches tl (i_e::matches_i) ((p, e)::checked_matches)
          | Error (i, msg) -> Error (i, msg)
          end
        | VariantDecomposition (i, v, c, names) ->
          begin match type_check e st with
          | Ok (i_e, e) -> type_check_matches tl (i_e::matches_i) ((p, e)::checked_matches)
          | Error (i, msg) -> Error (i, msg)
          end
        | ListDecomposition (i, hd_name, tl_name) ->
          begin match type_check e st with
          | Ok (i_e, e) -> type_check_matches tl (i_e::matches_i) ((p, e)::checked_matches)
          | Error (i, msg) -> Error (i, msg)
          end
        | TupleDecomposition (i, names) ->
          begin match type_check e st with
          | Ok (i_e, e) -> type_check_matches tl (i_e::matches_i) ((p, e)::checked_matches)
          | Error (i, msg) -> Error (i, msg)
          end
        | p ->
          begin match type_check p st with
          | Ok (i_p, p) ->
            begin match st#get_function_restype "·=·" i_p._type with
            | Some res_type ->
              begin match type_check e st with
              | Ok (i_e, e) -> type_check_matches tl (i_e::matches_i) ((p, e)::checked_matches)
              | Error (i, msg) -> Error (i, msg)
              end
            | None -> Error (i, "A match pattern must be a variant decomposition, a list decomposition, a tuple decomposition, the symbol \"_\" or a type with the operator ·=· defined.")
            end
          | Error (i, msg) -> Error (i, msg)
          end
        end
    in
    let m_st = st#next_child in
    begin match type_check m m_st with
    | Ok (i_m, m) ->
      begin match type_check_matches matches [] [] with
      | Ok (i_matches, matches) ->
        let type_list = List.map (fun x -> x._type) i_matches in
        let union_types = List.fold_left (fun t1 t2 -> Union (t1, t2)) (List.hd type_list) (List.tl type_list) in
        let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=union_types} in
                Ok (i, Match(i, m, matches))
      | Error (i, e) -> Error (i, e)
      end
    | Error (i, e) -> Error (i, e)
    end


  | AnyMatch i -> Ok (i, AnyMatch i)


  | IntLiteral (i, x) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=Int} in
                         Ok (i, IntLiteral (i, x))


  | RealLiteral (i, x) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=Real} in
                          Ok (i, RealLiteral (i, x))


  | StringLiteral (i, s) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=String} in
                            Ok (i, StringLiteral (i, s))


  | AtomLiteral (i, n) -> let type_name = String.split_on_char '_' n |>
                          List.map (fun s -> String.capitalize_ascii s) |>
                          String.concat ""
                          in
                          let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(SpecificAtom type_name)} in
                          Ok (i, AtomLiteral (i, n))


  | BoolLiteral (i, b) -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=Bool} in
                          Ok (i, BoolLiteral (i, b))


  | TupleLiteral (i, exp_list) ->
    let rec tuple_type_check expressions checked_expressions types =
      begin match expressions with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(Tuple (List.rev types))} in
              Ok (i, Sequence (i, (List.rev checked_expressions)))
      | e::l -> begin match type_check e st with
                | Ok (i, e) -> tuple_type_check l (e::checked_expressions) (i._type::types)
                | Error (i, s) -> Error (i, s)
                end
      end
    in
    tuple_type_check exp_list [] []


  | ListLiteral (i, exp_list) ->
    let rec list_type_check expressions checked_expressions _type =
      begin match expressions with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(List _type)} in
              Ok (i, ListLiteral (i, (List.rev checked_expressions)))
      | e::l -> begin match type_check e st with
                | Ok (i, e) -> list_type_check l (e::checked_expressions) (Union (i._type, _type))
                | Error (i, s) -> Error (i, s)
                end
      end
    in
    list_type_check exp_list [] Void


  | ListDecomposition (i, h, t) -> Ok (i, ListDecomposition (i, h, t))


  | VectorLiteral (i, exp_list) ->
    let rec vector_type_check expressions checked_expressions _type =
      begin match expressions with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(Vector _type)} in
              Ok (i, VectorLiteral (i, (List.rev checked_expressions)))
      | e::l -> begin match type_check e st with
                | Ok (i, e) -> vector_type_check l (e::checked_expressions) (Union (i._type, _type))
                | Error (i, s) -> Error (i, s)
                end
      end
    in
    vector_type_check exp_list [] Void


  | MatrixLiteral (i, exp_matrix) ->
    let rec row_type_check expressions checked_expressions _type =
      match expressions with
      | [] -> Ok (checked_expressions, _type)
      | hd::tl ->
        begin match type_check hd st with
        | Ok (i, e) -> row_type_check tl (e::checked_expressions) (Union (i._type, _type))
        | Error (i, s) -> Error (i, s)
        end
    in
    let rec matrix_type_check expressions checked_expressions _type =
      begin match expressions with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(Matrix _type)} in
              Ok (i, MatrixLiteral (i, (List.rev checked_expressions)))
      | hd::tl ->
        begin match row_type_check hd [] Void with
        | Ok (e, t) -> matrix_type_check tl (e::checked_expressions) (Union (t, _type))
        | Error (i, s) -> Error (i, s)
        end
      end
    in
    matrix_type_check exp_matrix [[]] Void


  | DictionaryLiteral (i, key_values) ->
    let rec key_values_type_check key_values checked_key_values key_type value_type =
      match key_values with
      | [] -> let i = {start_pos=i.start_pos; end_pos=i.end_pos; _type=(Dictionary (key_type, value_type))} in
              Ok (i, DictionaryLiteral (i, (List.rev checked_key_values)))
      | (k, v)::tl ->
        begin match type_check k st with
        | Ok (ik, k) ->
          begin match st#get_function_restype "·=·" (Tuple [ik._type; ik._type]) with
          | Some res_type ->
            if res_type = Bool then
              begin match type_check v st with
              | Ok (iv, v) -> key_values_type_check tl ((k, v)::checked_key_values) (Union (ik._type, key_type)) (Union (iv._type, value_type))
              | Error (i, s) -> Error (i, s)
              end
            else Error (ik, Printf.sprintf "A key expression must have the ·=· operator defined and returning a Bool expression, but the type %s doesn't have it."
                                           (string_of_type_expression ik._type))
          | None -> Error (ik, Printf.sprintf "A key expression must have the ·=· operator defined, but the type %s doesn't have it."
                                              (string_of_type_expression ik._type))
          end
        | Error (i, s) -> Error (i, s)
        end
    in
    key_values_type_check key_values [] Void Void


  | _ -> Ok ({start_pos=(0, 0, 0); end_pos=(0, 0, 0); _type=Unknown}, e)
