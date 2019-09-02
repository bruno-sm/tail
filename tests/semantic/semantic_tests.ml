open Alcotest
open Typechecker
open Ast


let test_subtyping () =
  (check bool) "Int <: Real" true (Int <= Real);
  (check bool) "S -> T1 and S -> T2 <: S -> (T1 and T2)" true
               (Intersection(
                 Arrow(SpecificAtom("S"), SpecificAtom("T1")),
                 Arrow(SpecificAtom("S"), SpecificAtom("T2")))
                <=
                Arrow(
                 SpecificAtom("S"),
                 Intersection(SpecificAtom("T1"), SpecificAtom("T2"))));
  (check bool) "not T !<: T" false (Complement(SpecificAtom("T")) <= SpecificAtom("T"));
  (check bool) "T !<: not T" false (SpecificAtom("T") <= Complement(SpecificAtom("T")));
  (check bool) "T1 <: T1 or T2" true (SpecificAtom("T1") <= Union(SpecificAtom("T1"), SpecificAtom("T2")));
  (check bool) "T1 or T2 !<: T1" false (Union(SpecificAtom("T1"), SpecificAtom("T2")) <= SpecificAtom("T1"));
  (check bool) "T1 !<: T1 and T2" false (SpecificAtom("T1") <= Intersection(SpecificAtom("T1"), SpecificAtom("T2")));
  (check bool) "T1 and T2 <: T1" true (Intersection(SpecificAtom("T1"), SpecificAtom("T2")) <= SpecificAtom("T1"));
  (check bool) "Unknown <: Unknown or T1" true (Unknown <=~ Union(Unknown, SpecificAtom("T1")));
  (check bool) "Unknown and T1 <: T1" true (Intersection(Unknown, SpecificAtom("T1")) <=~ Unknown)


let test_aplicative_concretization () =
  (check bool) "gamma+((? -> Bool) and (Int -> Int)) = [[? → Bool; Int → Int]]" true
               ((aplicative_concretization_plus (Intersection(Arrow(Unknown, Bool), Arrow(Int, Int))))
               = [[Arrow(Unknown, Bool); Arrow(Int, Int)]])

let test_set = [
  "Test subtyping", `Quick, test_subtyping;
  "Test aplicative concretization", `Quick, test_aplicative_concretization
]
