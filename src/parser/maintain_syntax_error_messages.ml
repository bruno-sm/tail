(* Script to update and mantain the syntax_errors.messages file *)
#use "topfind"
#thread
(* #require "ppx_jane,core" *)
#require "shexp.process"
open Shexp_process
open Shexp_process.Infix


let main : unit t =
  file_exists "syntax_errors.messages" >>= function
  | true ->
    run "menhir" ["--update-errors"; "syntax_errors.messages"; "syntax.mly"] |> stdout_to "syntax_errors_aux.messages"
    >> rename "syntax_errors_aux.messages" "syntax_errors.messages"
    >> run "menhir" ["--list-errors"; "syntax.mly"] |> stdout_to "listed_errors.messages"
    >> run "menhir" ["--compare-errors"; "listed_errors.messages";
                     "--compare-errors"; "syntax_errors.messages"; "syntax.mly"]
    >> rm "listed_errors.messages"
    >> echo "menhir --update-errors syntax_errors.messages syntax.mly > syntax_errors.messages"
    >> echo "menhir --list-errors syntax.mly > listed_errors.messages"
    >> echo "menhir --compare-errors listed_errors.message --compare-errors syntax_errors.messages syntax.mly"
  | false ->
    run "menhir" ["--list-errors"; "syntax.mly"] |> stdout_to "syntax_errors.messages"
    >> echo "menhir --list-errors syntax.mly > syntax_errors.messages"


let () =
  eval main
