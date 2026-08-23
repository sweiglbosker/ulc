open Ulc.Term

let check name expected actual = if expected <> actual then failwith ("failed: " ^ name)

let test_free_variables () =
  let make_free_var_test (name, term, expected) =
    Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string)) name expected (StringSet.elements (free term)))
  and free_cases =
    [ "variable", Var "x", [ "x" ]
    ; "bound variable", Abs ("x", Var "x"), []
    ; "free in abstraction", Abs ("x", App (Var "x", Var "y")), [ "y" ]
    ]
  in
  [ "free_variables", List.map make_free_var_test free_cases ]
;;

let () = Alcotest.run "term" (test_free_variables ())
