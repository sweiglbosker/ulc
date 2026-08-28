module E = Ulc.Eval
module T = Ulc.Term

let term = Alcotest.testable T.pp ( = )
let optional_term = Alcotest.option term

let step_tests () =
  let make_test (name, input, expected) =
    Alcotest.test_case name `Quick (fun () ->
      Alcotest.check optional_term name expected (E.step input))
  and cases =
    [ "value", T.Abs ("x", T.Var "x"), None
    ; "variable", T.Var "x", None
    ; "id", T.App (T.Abs ("y", T.Var "y"), T.Abs ("x", T.Var "x")), Some (T.Abs ("x", T.Var "x"))
    ]
  in
  "step", List.map make_test cases
;;

let () = Alcotest.run "eval" [ step_tests() ]
