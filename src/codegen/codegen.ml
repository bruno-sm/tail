open Llvm
open Ast

exception CompileError of string


let rec codegen_expr context builder llmod ast : llvalue option =
  let value e =
    match codegen_expr context builder llmod e with
    | Some v -> v
    | None -> raise (CompileError (Printf.sprintf "Expression doesn't reach a value:\n%s" (string_of_expression e)))
  in
  let named_type n =
    match type_by_name llmod n with
    | Some t -> t
    | None -> raise (CompileError (Printf.sprintf "Type %s is not defined in llcontext" n))
  in
  match ast with
  | Sequence (i, l) ->
    let l = List.map (codegen_expr context builder llmod) l in
    List.nth l ((List.length l) - 1)
  | Assignment (i, v, e) ->
    begin match i._type with
    | Int ->
      let v = build_alloca (i64_type context) v builder in
      build_store (value e) v builder |> ignore;
      None
    | Rational ->
      let rt = named_type "rational" in
      let v = build_alloca rt v builder in
      build_store (value e) v builder |> ignore;
      None
    | Real ->
      let v = build_alloca (double_type context) v builder in
      build_store (value e) v builder |> ignore;
      None
    | Complex ->
      let rt = named_type "complex" in
      let v = build_alloca rt v builder in
      build_store (value e) v builder |> ignore;
      None
    end
  | BinOp (_, op, e1, e2) ->
    begin match op with
    | Frac ->
      let n1 = value e1 in
      let n2 = value e2 in
      let rt = named_type "rational" in
      let v = const_named_struct rt [|n1; n2|] in Some v
    | _ -> None
    end
  | PostfixOp (_, op, e) ->
    begin match op with
    | I ->
      let n1 = const_float (double_type context) 0.0 in
      let n2 = value e in
      let rt = named_type "complex" in
      let v = const_named_struct rt [|n1; n2|] in Some v
    | _ -> None
    end
  | IntLiteral (_, n) ->
    let v = const_int (i64_type context) n in Some v
  | RealLiteral (_, n) ->
    let v = const_float (double_type context) n in Some v
  | _ -> None


let compile ast file =
  print_endline "Compilando...";
  let context = global_context () in
  let llmod = create_module context "tail compiler" in
  (*codegen_expr context builder ast;*)
  let rational_type = named_struct_type context "rational" in
  let _ = struct_set_body rational_type [|(double_type context); (double_type context)|] false in
  let complex_type = named_struct_type context "complex" in
  let _ = struct_set_body complex_type [|(double_type context); (double_type context)|] false in
  let i64_t = i64_type context in
  let fty = function_type i64_t [| |] in
  let f = define_function "main" fty llmod in
  let builder = builder_at_end context (entry_block f) in
  let _ = codegen_expr context builder llmod ast in
  let _ = build_ret (const_int i64_t 0) builder in
  dump_module (llmod);
  Ok ()
