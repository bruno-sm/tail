module L = Taillexer

module P = Tailparser

module I = Tailparser.MenhirInterpreter


type command = CmdDefault
             | CmdDebug



(* Compiler *)
let rec tailc source output command =
  try
    let lexbuf = Sedlexing.Utf8.from_channel @@ open_in source in
    Printf.printf "source: %s\noutput: %s\n" source output;
    match command with
      | CmdDefault ->
        begin try
          compile lexbuf
        with
        | Tailparser.Error -> Printf.printf "Tailparser error in %s" (Sedlexing.Utf8.lexeme lexbuf)
        end

      | CmdDebug -> Taillexer.show_lexing lexbuf
  with
    | Sys_error msg -> print_endline msg


and compile lexbuf =
  parse lexbuf

and parse lexbuf =
  let rec parse_loop lexbuf context checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
      let token = L.lexer context lexbuf in
      let (start_pos, curr_pos) = Sedlexing.lexing_positions lexbuf in
      let checkpoint = I.offer checkpoint (token, start_pos, curr_pos) in
      Printf.printf "Passing token %s (line %d, column %d)\n"
        (L.string_of_token token)
        curr_pos.pos_lnum (curr_pos.pos_cnum - curr_pos.pos_bol + 1);
      parse_loop lexbuf context checkpoint

    | I.Shifting (_, _, request) ->
      Printf.printf "Shifting\n";
      let checkpoint = I.resume checkpoint in
      parse_loop lexbuf context checkpoint

    | I.AboutToReduce _ ->
      Printf.printf "About to reduce\n";
      let prev_cehckpoint = checkpoint in
      let checkpoint = I.resume checkpoint in
      parse_loop lexbuf context checkpoint

    | I.HandlingError env ->
      let (_, curr_pos) = Sedlexing.lexing_positions lexbuf in
      Printf.fprintf stderr
        "Syntax error at line %d\n"
        curr_pos.pos_lnum

    | I.Accepted v -> v |> Ast.string_of_expression |> print_endline

    | I.Rejected -> Printf.printf "Input rejected by the parser\n"
  in
  let context = Taillexer.new_context () in
  let (start_position, _) = Sedlexing.lexing_positions lexbuf in
  let start_checkpoint = Tailparser.Incremental.parse start_position in
  parse_loop lexbuf context start_checkpoint


(* Command line arguments using cmdliner *)
open Cmdliner


(* tailc info term *)
let info =
  let doc = "The tail programming language compiler." in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to bruno-sm at <bru.1bruno@gmail.com>."]
  in
  Term.info "tailc" ~version:"0.1.0" ~doc ~exits:Term.default_exits ~man


(* Argument for the source file name *)
let source =
  let doc = "Tail source file to compile." in
  Arg.(required & opt (some file) None & info ["s"; "source"] ~docv:"SOURCE_FILE" ~doc)


(* Argument for the output file name *)
let output =
  let doc = "Name of the executable file." in
  Arg.(value & opt string "program" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)


(* debug command info term *)
let debug_info =
  let doc = "Command to retrieve information about tailc execution." in
  Term.info "debug" ~doc ~exits:Term.default_exits


let () =
  let tailc_t = Term.(const tailc $ source $ output $ const CmdDefault) in
  let debug_t = Term.(const tailc $ source $ output $ const CmdDebug) in
  Term.exit @@ Term.eval_choice (tailc_t, info) [(debug_t, debug_info)]
