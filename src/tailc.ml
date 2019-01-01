module L = Taillexer

module P = Tailparser

module I = Tailparser.MenhirInterpreter


type command = CmdDefault
             | CmdDebug


(* Compiler *)
let rec tailc source output command =
  try
    let lexbuf = Sedlexing.Utf8.from_channel @@ open_in source in
    Sedlexing.set_filename lexbuf source;
    Printf.printf "source: %s\noutput: %s\n" source output;
    match command with
      | CmdDefault -> compile lexbuf

      | CmdDebug -> Taillexer.show_lexing lexbuf
  with
    | Sys_error msg -> print_endline msg


and compile lexbuf =
  parse lexbuf


and parse lexbuf =
  let context = Taillexer.new_context () in
  let succeed v = Ast.string_of_expression v |> print_endline in
  let fail lexbuf checkpoint =
    match checkpoint with
    | I.HandlingError env ->
      P.print_syntax_error env lexbuf
    | _ ->
      Printf.fprintf stderr "Program rejected by the parser"
  in
  let supplier = Sedlexing.with_tokenizer (Taillexer.lexer context) lexbuf in
  let (start_position, _) = Sedlexing.lexing_positions lexbuf in
  let start_checkpoint = P.Incremental.parse start_position in
  I.loop_handle succeed (fail lexbuf) supplier start_checkpoint


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
