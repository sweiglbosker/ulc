module P = Ulc.Parser
module T = Ulc.Term
module Tok = Ulc.Token

let phrase_testable = Alcotest.testable P.pp_phrase ( = )
let error_testable = Alcotest.testable P.pp_error ( = )
let result_testable = Alcotest.result phrase_testable error_testable

let parser_tests () =
  let make_test (name, buf, expected) =
    Alcotest.test_case name `Quick (fun () ->
      let actual = P.parse_source buf in
      Alcotest.check result_testable name expected actual)
  and cases =
    [ "variable", "x", Ok (P.Expression (T.Var "x"))
    ; "application", "f x", Ok (P.Expression (T.App (T.Var "f", T.Var "x")))
    ; ( "left-associative application"
      , "f x y"
      , Ok (P.Expression (T.App (T.App (T.Var "f", T.Var "x"), T.Var "y"))) )
    ; "parentheses", "(x)", Ok (P.Expression (T.Var "x"))
    ; ( "parenthesized application"
      , "f (x y)"
      , Ok (P.Expression (T.App (T.Var "f", T.App (T.Var "x", T.Var "y")))) )
    ; "ascii abstraction", "\\x.x", Ok (P.Expression (T.Abs ("x", T.Var "x")))
    ; "unicode abstraction", "λx.x", Ok (P.Expression (T.Abs ("x", T.Var "x")))
    ; ( "abstraction body"
      , "\\x.x y"
      , Ok (P.Expression (T.Abs ("x", T.App (T.Var "x", T.Var "y")))) )
    ; ( "apply abstraction"
      , "(\\x.x) y"
      , Ok (P.Expression (T.App (T.Abs ("x", T.Var "x"), T.Var "y"))) )
    ; ( "abstraction argument"
      , "f (\\x.x)"
      , Ok (P.Expression (T.App (T.Var "f", T.Abs ("x", T.Var "x")))) )
    ; "definition", "let id = \\x.x", Ok (P.Definition ("id", T.Abs ("x", T.Var "x")))
    ; ( "missing closing parenthesis"
      , "(x"
      , Error (P.Unexpected_token { found = Tok.make 2 ~ed:2 Tok.Eof; expected = ")" }) )
    ; ( "missing abstraction dot"
      , "\\x x"
      , Error
          (P.Unexpected_token { found = Tok.make 3 ~ed:4 (Tok.Ident "x"); expected = "." })
      )
    ; ( "missing definition equals"
      , "let x \\x.x"
      , Error (P.Unexpected_token { found = Tok.make 6 ~ed:7 Tok.Lambda; expected = "=" })
      )
    ; ( "trailing token"
      , "x )"
      , Error
          (P.Unexpected_token { found = Tok.make 2 ~ed:3 Tok.RParen; expected = "<EOF>" })
      )
    ]
  in
  "parse", List.map make_test cases
;;

let () = Alcotest.run "parser" [ parser_tests () ]
