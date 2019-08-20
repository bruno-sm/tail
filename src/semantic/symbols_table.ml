open Ast
module Vect = Batteries.Vect

type entry = VariableEntry of string * type_expression * bool * bool (* name, type, initialized, constant *)
           | VariantEntry of string * variant_constructor list



class symbols_table (parent : (symbols_table ref) option) = object(self)
  val mutable children : symbols_table Vect.t = Vect.empty
  val mutable table : entry Vect.t = Vect.empty
  val mutable index = -1


  method add_child child =
      index <- index + 1;
      children <- Vect.append child children


  method update_child child = children <- Vect.set children index child


  method next_child =
    let c = Vect.get children (index + 1) in
      index <- index + 1;
      c


  method reset_child_position =
    index <- -1;
    Vect.iter (fun c -> c#reset_child_position) children


  method add_entry entry =
    table <- Vect.append entry table


  method update_entry old_entry new_entry =
    try
      let i = Vect.findi (fun x -> x = old_entry) table in
      table <- Vect.set table i new_entry
    with
    | Not_found -> match parent with
                   | Some p -> (!p)#update_entry old_entry new_entry
                   | None -> ()


   method find_variable_in_scope name =
     try
       Some (Vect.find (
         fun x -> match x with
                  | VariableEntry (n, _, _, _) -> n = name
                  | _ -> false
       ) table)
     with
     | Not_found -> None


  method find_variable name =
    match self#find_variable_in_scope name with
    | Some v -> Some v
    | None -> match parent with
              | Some p -> (!p)#find_variable name
              | None -> None


  method find_function name arg_type =
    let rec check_unknown t1 t2 =
      match t1 with
      | Unknown -> true
      | Tuple t1_list ->
        begin match t2 with
          | Tuple t2_list ->
            List.length t1_list = List.length t2_list &&
            List.fold_left2 (fun a b c -> a && (check_unknown b c)) true t1_list t2_list
          | _ -> false
        end
      | _ -> false
    in
    try
      Some (Vect.find (
        fun x -> match x with
                 | VariableEntry (n, t, true, _) -> n = name &&
                   begin match t with
                   | Arrow (t1, _) -> t1 = arg_type || check_unknown t1 arg_type
                   | _ -> false
                   end
                 | _ -> false
      ) table)
    with
    | Not_found -> match parent with
                   | Some p -> (!p)#find_variable name
                   | None -> None


  method find_variant_constructor vname cname =
    try
      Some (Vect.find (
        fun x -> match x with
                 | VariantEntry (v, c_list) ->
                  v = vname && List.exists (fun x -> x.name = cname) c_list
                 | _ -> false
      ) table)
    with
    | Not_found -> match parent with
                   | Some p -> (!p)#find_variant_constructor vname cname
                   | None -> None


  method get_function_restype name arg_type =
    match self#find_function name arg_type with
      | Some (VariableEntry (_, t, true, _)) ->
        begin match t with
        | Arrow (_, t2) -> Some t2
        | _ -> None
        end
      | _ -> None


  method to_string ident =
    let string_of_entry = function
      | VariableEntry (n, t, init, const) ->
        Printf.sprintf "%sVariableEntry(%s, %s, %b, %b)" ident n (string_of_type_expression t) init const
      | VariantEntry (n, _) ->
        Printf.sprintf "%sVariantEntry(%s)" ident n
    in
    let table_string_list = Vect.map (fun x -> string_of_entry x) table in
    let table_string = Vect.fold_left (fun a b -> a ^ "\n" ^ b) "" table_string_list in
    let children_string_list = Vect.map (fun x -> x#to_string (ident ^ "  ")) children in
    let children_string = Vect.fold_left (fun a b -> a ^ "\n\n" ^ b) "" children_string_list in
    Printf.sprintf "%s------------------\n%s\n%s\n\n%s================" ident table_string children_string ident

end
