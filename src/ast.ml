type info = {
  start_pos : int * int * int;
  end_pos : int * int * int;
  _type : type_expression;
}

and type_expression = Int
                     | Real
                     | Rational
                     | Complex
                     | String
                     | Atom
                     | SpecificAtom of string
                     | Bool
                     | Unknown
                     | Void
                     | Universe
                     | Variant of string
                     | VariantConstructor of string * string
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
                     | File
                     | WriteFile
                     | ReadFile


let pos_info s e = {start_pos=s; end_pos=e; _type=Unknown}


let rec string_of_type_expression = function
  | Int -> "Int"

  | Real -> "Real"

  | Rational -> "Rational"

  | Complex -> "Complex"

  | String -> "String"

  | Atom -> "Atom"

  | SpecificAtom s -> Printf.sprintf ":%s" s

  | Bool -> "Bool"

  | Unknown -> "Unknown"

  | Void -> "Void"

  | Universe -> "U"

  | Variant v -> Printf.sprintf "Variant(%s)" v

  | VariantConstructor (v, c) -> Printf.sprintf "Variant(%s::%s)" v c

  | Arrow (source, target) -> Printf.sprintf "%s -> %s"
                              (string_of_type_expression source)
                              (string_of_type_expression target)

  | Tuple types -> Printf.sprintf("(%s)")
                   (List.map string_of_type_expression types |>
                    List.fold_left (fun s1 s2 -> s1 ^ ", " ^ s2) "")

  | List t -> Printf.sprintf("List of %s") @@ string_of_type_expression t

  | Vector t -> Printf.sprintf("Vector of %s") @@ string_of_type_expression t

  | Matrix t -> Printf.sprintf("Matrix of %s") @@ string_of_type_expression t

  | Dictionary (t1, t2) -> Printf.sprintf("Dictionary of %s, %s")
                           (string_of_type_expression t1)
                           (string_of_type_expression t2)

  | Union (t1, t2) -> Printf.sprintf("%s or %s")
                      (string_of_type_expression t1)
                      (string_of_type_expression t2)

  | Intersection (t1, t2) -> Printf.sprintf("%s and %s")
                             (string_of_type_expression t1)
                             (string_of_type_expression t2)

  | Complement t -> Printf.sprintf("not %s") @@ string_of_type_expression t

  | ParenthesesType t -> Printf.sprintf("(%s)") @@ string_of_type_expression t

  | File -> "File"

  | WriteFile -> "WriteFile"

  | ReadFile -> "ReadFile"


type expression = Sequence of info * expression list
                | Unit
                | Parentheses of info * expression
                | Block of info * expression
                | BinOp of info * operator * expression * expression
                | PrefixOp of info * operator * expression
                | PostfixOp of info * operator * expression
                | Variable of info * string
                | Function of info * string * string list * expression
                | Lambda of info * string list * type_expression * expression
                | FunctionCall of info * expression * expression option
                | Annotation of info * string * type_expression
                | VariantDeclaration of info * string * variant_constructor list
                | VariantInstance of info * string * string * expression option
                | VariantProjection of info * expression * string
                | VariantDecomposition of info * string * string * string list
                | Assignment of info * string * expression
                | If of info * expression * expression *
                        expression list *
                        expression option
                | Elif of info * expression * expression
                | Else of info * expression
                | Match of info * expression * (expression * expression) list
                | AnyMatch of info
                | IntLiteral of info * int
                | RealLiteral of info * float
                | StringLiteral of info * string
                | AtomLiteral of info * string
                | BoolLiteral of info * bool
                | TupleLiteral of info * expression list
                | TupleDecomposition of info * string list
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
             | Not
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


let operator_name = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Frac -> "//"
  | Rem -> "%"
  | Exp -> "^"
  | I -> "i"
  | Typeof -> "typeof"
  | And -> "and"
  | Xor -> "xor"
  | Or -> "or"
  | Not -> "not"
  | Equal -> "="
  | Different -> "!="
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Projection _ -> "."


let rec string_of_expression ident = function
  | Unit -> "Unit"

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

  | PrefixOp (_, op, e) -> Printf.sprintf "PrefixOp(%s)\n%s| %s\n"
                          (string_of_operator ident op)
                          ident
                          (string_of_expression (ident ^ " ") e)

  | PostfixOp (_, op, e) -> Printf.sprintf "PostfixOp(%s)\n%s| %s\n"
                          (string_of_operator ident op)
                          ident
                          (string_of_expression (ident ^ " ") e)

  | Variable (_, v) -> Printf.sprintf "Variable\n%s| %s\n" ident v

  | Function (_, n, args, e) -> Printf.sprintf "Function(%s)(%s)\n%s| %s\n"
                             n (List.fold_left (fun a b -> a ^ ", " ^ b) "" args) ident
                             (string_of_expression ident e)

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

  | Assignment (_, name, e) -> Printf.sprintf "Assignment(%s)\n%s| %s\n"
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
  | Not -> "Not"
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



(* Operations over types -------------------------------------------------------*)

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
  | (ParenthesesType t11, t2) -> t11 <= t2
  | (t1, ParenthesesType t21) -> t1 <= t21
  | (Arrow (t1, t2), Arrow (s1, s2)) -> s1 <= t1 && t2 <= s1
  | (Tuple l1, Tuple l2) -> List.fold_left2 (fun x t1 t2 -> x && t1 <= t2) true l1 l2
  | (Union(ParenthesesType t11, t2), t) -> Union(t11, t2) <= t
  | (Union(t1, ParenthesesType t21), t) -> Union(t1, t21) <= t
  | (Union(t11, t12), t2) -> t11 <= t2 && t12 <= t2
  | (t1, Union(t21, t22)) -> t1 <= t21 || t1 <= t22
  | (Intersection(ParenthesesType(t11), t2), t) -> Intersection(t11, t2) <= t
  | (Intersection(t1, ParenthesesType (t21)), t) -> Intersection(t1, t21) <= t
  | (Intersection(Union(s1, s2), Union(t1, t2)), t) -> (not (s1 <= Union(t1, t2)) || s1 <= t) &&
                                                       (not (s2 <= Union(t1, t2)) || s2 <= t) &&
                                                       (not (t1 <= Union(s1, s2)) || t1 <= t) &&
                                                       (not (t2 <= Union(s1, s2)) || t2 <= t)
  | (Intersection(t11, t12), t2) -> (not (t11 <= t12) || t11 <= t2) &&
                                    (not (t12 <= t11) || t12 <= t2)
  | (t1, Intersection(t21, t22)) -> t1 <= t21 && t1 <= t22
  | (Complement (Union (t11, t12)), t21) -> Intersection(Complement(t11), Complement(t12)) <= t21
  | (Complement t11, Union(t21, t22)) -> not (t11 <= t21 || t11 <= t22)
  | (Complement (Complement t11), t2) -> t11 <= t2
  | (t1, Complement t21) -> Intersection(t1, t21) <= Void
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
