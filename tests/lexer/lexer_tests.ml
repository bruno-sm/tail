open Alcotest
open Taillexer
open Tailparser

let token = testable (Fmt.of_to_string string_of_token) (=)



let test_blocks () =
  let context = new_context () in
  let test_string = "\n\n\n\t\n\n\n\t\n\n" in
  let lexbuf = Sedlexing.Utf8.from_string test_string in
  check token "Beginning of block" (BLOCK_BEGIN 1) (lexer context lexbuf);
  check token "Beginning of block" (BLOCK_BEGIN 1) (lexer context lexbuf);
  check token "End of block" (BLOCK_END 1) (lexer context lexbuf);
  check token "End of block" (BLOCK_END 1) (lexer context lexbuf);
  check token "End of file" EOF (lexer context lexbuf)


let test_set = [
  "Test blocks", `Quick, test_blocks
]


let () =
  run "Lexer tests" [
    "Lexer test set", test_set;
  ]
