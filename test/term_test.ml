open Ulc.Term

let term_testable = Alcotest.testable pp ( = )

let free_variables_tests () =
  let make_test (name, term, expected) =
    Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string)) name expected (StringSet.elements (free term)))
  and cases =
    [ "variable", Var "x", [ "x" ]
    ; "repeated variable", App (Var "x", Var "x"), [ "x" ]
    ; "two free variables", App (Var "x", Var "y"), [ "x"; "y" ]
    ; "bound variable", Abs ("x", Var "x"), []
    ; "free in abstraction", Abs ("x", App (Var "x", Var "y")), [ "y" ]
    ; "free occurrence outside abstraction", App (Var "x", Abs ("x", Var "x")), [ "x" ]
    ; ( "nested abstractions"
      , Abs ("x", Abs ("y", App (App (Var "x", Var "y"), Var "z")))
      , [ "z" ] )
    ]
  in
  "free_variables", List.map make_test cases
;;

let substitution_tests () =
  let make_test (name, variable, replacement, target, expected) =
    Alcotest.test_case name `Quick (fun () ->
      let actual = substitute variable replacement target in
      Alcotest.check term_testable name expected actual)
  and cases =
    [ "matching variable", "x", Var "z", Var "x", Var "z"
    ; "different variable", "x", Var "z", Var "y", Var "y"
    ; "term replacement", "x", Abs ("z", Var "z"), Var "x", Abs ("z", Var "z")
    ; "application", "x", Var "z", App (Var "x", Var "x"), App (Var "z", Var "z")
    ; "shadowed variable", "x", Var "z", Abs ("x", Var "x"), Abs ("x", Var "x")
    ; "safe abstraction", "x", Var "z", Abs ("y", Var "x"), Abs ("y", Var "z")
    ; ( "nested shadowing"
      , "x"
      , Var "z"
      , Abs ("y", Abs ("x", App (Var "x", Var "y")))
      , Abs ("y", Abs ("x", App (Var "x", Var "y"))) )
    ; "avoid capture", "x", Var "y", Abs ("y", Var "x"), Abs ("y'", Var "y")
    ; ( "rename bound occurrences"
      , "x"
      , Var "y"
      , Abs ("y", App (Var "x", Var "y"))
      , Abs ("y'", App (Var "y", Var "y'")) )
    ; ( "skip occupied fresh name"
      , "x"
      , App (Var "y", Var "y'")
      , Abs ("y", Var "x")
      , Abs ("y''", App (Var "y", Var "y'")) )
    ]
  in
  "substitution", List.map make_test cases
;;

let () = Alcotest.run "term" [ free_variables_tests (); substitution_tests () ]
