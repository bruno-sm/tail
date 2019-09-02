let test_suites: unit Alcotest.test list = [
  "Semantic", Semantic_tests.test_set;
]

let () = Alcotest.run "tail" test_suites
