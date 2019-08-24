open Ast
module Vect = Batteries.Vect

type entry = VariableEntry of string * type_expression * bool (* name, type, initialized*)
           | VariantEntry of string * variant_constructor list
           | FunctionEntry of string * type_expression * type_expression



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


   method remove_entry_in_scope entry =
     try
       let i = Vect.findi (fun x -> x = entry) table in
       table <- Vect.remove i 1 table
     with
     | Not_found -> ()


   method find_variable_in_scope name =
     try
       Some (Vect.find (
         fun x -> match x with
                  | VariableEntry (n, _, _) -> n = name
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


  method find_function_by_name name =
    try
      Some (Vect.find (
        fun x -> match x with
                 | FunctionEntry (n, _, _) -> n = name
                 | _ -> false
      ) table)
    with
    | Not_found -> match parent with
                   | Some p -> (!p)#find_function_by_name name
                   | None -> None


  method find_function name arg_type =
    let rec compatible_arg_type st arg_type =
      match (st, arg_type) with
      | (Tuple st_l, Tuple arg_type_l) ->
        List.map2 (fun t1 t2 -> compatible_arg_type t1 t2) st_l arg_type_l |>
        List.fold_left (fun x y -> x && y) true
      | _ -> arg_type = Unknown || st = Unknown || st = arg_type
    in
    try
      Some (Vect.find (
        fun x -> match x with
                 | FunctionEntry (n, st, dt) -> n = name &&
                                                compatible_arg_type st arg_type
                 | _ -> false
      ) table)
    with
    | Not_found -> match parent with
                   | Some p -> (!p)#find_function name arg_type
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
      | Some (FunctionEntry (n, st, dt)) -> Some dt
      | _ -> None


  method to_string ident =
    let string_of_entry = function
      | VariableEntry (n, t, init) ->
        Printf.sprintf "%sVariableEntry(%s, %s, %b)" ident n (string_of_type_expression t) init
      | VariantEntry (n, _) ->
        Printf.sprintf "%sVariantEntry(%s)" ident n
      | FunctionEntry (n, st, dt) ->
        Printf.sprintf "%sFunctionEntry(%s, %s)" ident n (string_of_type_expression (Arrow(st, dt)))
    in
    let table_string_list = Vect.map (fun x -> string_of_entry x) table in
    let table_string = Vect.fold_left (fun a b -> a ^ "\n" ^ b) "" table_string_list in
    let children_string_list = Vect.map (fun x -> x#to_string (ident ^ "  ")) children in
    let children_string = Vect.fold_left (fun a b -> a ^ "\n\n" ^ b) "" children_string_list in
    Printf.sprintf "%s------------------\n%s\n%s\n\n%s================" ident table_string children_string ident

end
