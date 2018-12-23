type command = CmdDefault
             | CmdDebug


(* Compiler *)
let tailc source output command =
  try
    let lexbuf = Sedlexing.Utf8.from_channel @@ open_in source in
    match command with
      | CmdDefault -> Parser.parse Lexer.lexer lexbuf
      | CmdDebug -> Lexer.show_lexing lexbuf
  with
    | Sys_error msg -> print_endline msg


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
  Arg.(required & pos ~rev:true 0 (some file) None & info [] ~docv:"SOURCE_FILE" ~doc)


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
