open Llvm
open Ast

exception CompileError of string


let rec codegen_expr context builder llmod ast : llvalue =
  let char_type = i8_type context in
  let unit_type = i8_type context in
  let const_unit = const_int unit_type 0 in
  let value = codegen_expr context builder llmod in
  let named_type n =
    match type_by_name llmod n with
    | Some t -> t
    | None -> raise (CompileError (Printf.sprintf "Type %s is not defined in llcontext" n))
  in
  match ast with
  | Sequence (i, l) ->
    let l = List.map (codegen_expr context builder llmod) l in
    List.nth l ((List.length l) - 1)
  | Block (_, e) -> value e
  | Parentheses (_, e) -> value e
  | Assignment (i, n, e) ->
    let v = value e in
    let t = type_of v in
    let n = build_alloca t n builder in
    build_store v n builder |> ignore;
    v
(* Meter prototipados en una primera pasada y añadir bloques aquí
  | Function (i, n, args, e) ->
    let art = List.length args in
    let fty = function_type (i8_type context) (Array.create art (i8_type context)) in
    let f = define_function n fty llmod in
    position_at_end (entry_block f) builder;
    let ret_val = codegen_expr context builder llmod e in
    build_ret ret_val builder |> ignore;
    const_unit*)
(* | FunctionCall (_, e1, e2) ->
    begin match e1 with
    | Varaible("write") ->
      begin match e2 with
      | s -> expr
      | TupleLiteral (s::f) -> const_unit
      end
    | _ -> const_unit
    end*)
  | BinOp (_, op, e1, e2) ->
    begin match op with
    | Frac ->
      let n1 = value e1 in
      let n2 = value e2 in
      let rt = named_type "rational" in
      const_named_struct rt [|n1; n2|]
    | _ -> const_unit
    end
  | PostfixOp (_, op, e) ->
    begin match op with
    | I ->
      let n1 = const_float (double_type context) 0.0 in
      let n2 = value e in
      let rt = named_type "complex" in
      const_named_struct rt [|n1; n2|]
    | _ -> const_unit
    end
  | IntLiteral (_, n) -> const_int (i64_type context) n
  | RealLiteral (_, n) -> const_float (double_type context) n
  | StringLiteral (_, s) ->
    let char_vector = String.to_seq s |>
                      Array.of_seq |>
                      Array.map (fun c -> const_int (i8_type context) (Char.code c))
    in
    const_vector char_vector
  | _ -> const_unit


let compile ast file =
  print_endline "Compilando...";
  let context = global_context () in
  let llmod = create_module context "tail compiler" in
  (*codegen_expr context builder ast;*)
  let rational_type = named_struct_type context "rational" in
  let _ = struct_set_body rational_type [|(double_type context); (double_type context)|] false in
  let complex_type = named_struct_type context "complex" in
  let _ = struct_set_body complex_type [|(double_type context); (double_type context)|] false in
  let putsType = function_type (i32_type context) [|(pointer_type (i8_type context))|] in
  let f = declare_function "puts" putsType llmod in
  let i64_t = i64_type context in
  let fty = function_type i64_t [| |] in
  let f = define_function "main" fty llmod in
  let builder = builder_at_end context (entry_block f) in
  let _ = codegen_expr context builder llmod ast in
  let _ = build_ret (const_int i64_t 0) builder in
  dump_module (llmod);
  Ok ()
