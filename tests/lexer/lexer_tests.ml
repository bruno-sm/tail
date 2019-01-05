open Alcotest
open Taillexer
open Tailparser

let token = testable (Fmt.of_to_string string_of_token) (=)



let test_simple_blocks () =
  let context = new_context () in
  let test_string = "\n\n\n\t\n\n\n\t\n\n" in
  let lexbuf = Sedlexing.Utf8.from_string test_string in
  check token "Beginning of block" BLOCK_BEGIN (lexer context lexbuf);
  check token "End of block" BLOCK_END (lexer context lexbuf);
  check token "End of file" EOF (lexer context lexbuf)


let test_several_blocks () =
  let context = new_context () in
  let test_string = "\n\n\n\t\nx\n\n\t\n\n" in
  let lexbuf = Sedlexing.Utf8.from_string test_string in
  check token "Beginning of first block" BLOCK_BEGIN (lexer context lexbuf);
  check token "End of first block" BLOCK_END (lexer context lexbuf);
  check token "Variable" (NAME "x") (lexer context lexbuf);
  check token "Beginning of second block" BLOCK_BEGIN (lexer context lexbuf);
  check token "End of second block" BLOCK_END (lexer context lexbuf);
  check token "End of file" EOF (lexer context lexbuf)


let test_nested_blocks () =
  let context = new_context () in
  let test_string = "\n\n\n\t\t\n\t\t\n\t\n\t\n\n" in
  let lexbuf = Sedlexing.Utf8.from_string test_string in
  check token "Beginning of first block" BLOCK_BEGIN (lexer context lexbuf);
  check token "Beginning of second block" BLOCK_BEGIN (lexer context lexbuf);
  check token "End of second block" BLOCK_END (lexer context lexbuf);
  check token "End of first block" BLOCK_END (lexer context lexbuf);
  check token "End of file" EOF (lexer context lexbuf)


let test_blocks_closed_at_eof () =
  let context = new_context () in
  let test_string = "\n\n\n\t\t\t\n\n" in
  let lexbuf = Sedlexing.Utf8.from_string test_string in
  check token "Beginning of first block" BLOCK_BEGIN (lexer context lexbuf);
  check token "Beginning of second block" BLOCK_BEGIN (lexer context lexbuf);
  check token "Beginning of third block" BLOCK_BEGIN (lexer context lexbuf);
  check token "End of third block" BLOCK_END (lexer context lexbuf);
  check token "End of second block" BLOCK_END (lexer context lexbuf);
  check token "End of first block" BLOCK_END (lexer context lexbuf);
  check token "End of file" EOF (lexer context lexbuf)


let block_tests = [
  "Test simple blocks", `Quick, test_simple_blocks;
  "Test several blocks", `Quick, test_several_blocks;
  "Test nested blocks", `Quick, test_nested_blocks;
  "Test blocks closed at eof", `Quick, test_blocks_closed_at_eof;
]


let () =
  run "Lexer tests" [
    "Block tests", block_tests;
  ]
