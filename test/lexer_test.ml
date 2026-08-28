module L = Ulc.Lexer
module T = Ulc.Token

let kind_testable = Alcotest.testable T.pp_kind ( = )
let kinds_testable = Alcotest.list kind_testable

let kind_tests () =
  let make_test (name, buf, expected) =
    Alcotest.test_case name `Quick (fun () ->
      let kinds = L.scan buf |> List.map (fun (t : T.t) -> t.kind) in
      Alcotest.check kinds_testable name expected kinds)
  and cases =
    [ "empty", "", [ T.Eof ]
    ; "whitespace", " \t\n\r", [ T.Eof ]
    ; "ascii lambda", "\\", [ T.Lambda; T.Eof ]
    ; "unicode lambda", "λ", [ T.Lambda; T.Eof ]
    ; "punctuation", ".()=", [ T.Dot; T.LParen; T.RParen; T.Equals; T.Eof ]
    ; "identifier", "hello", [ T.Ident "hello"; T.Eof ]
    ; "uppercase identifier", "Hello", [ T.Ident "Hello"; T.Eof ]
    ; "identifier continuations", "foo_2'", [ T.Ident "foo_2'"; T.Eof ]
    ; "let keyword", "let", [ T.Let; T.Eof ]
    ; "keyword prefix", "letter", [ T.Ident "letter"; T.Eof ]
    ; "keyword with suffix", "let2", [ T.Ident "let2"; T.Eof ]
    ; "separate identifiers", "foo bar", [ T.Ident "foo"; T.Ident "bar"; T.Eof ]
    ; "ascii abstraction", "\\x.x", [ T.Lambda; T.Ident "x"; T.Dot; T.Ident "x"; T.Eof ]
    ; "unicode abstraction", "λx.x", [ T.Lambda; T.Ident "x"; T.Dot; T.Ident "x"; T.Eof ]
    ; ( "parenthesized application"
      , "(foo bar)"
      , [ T.LParen; T.Ident "foo"; T.Ident "bar"; T.RParen; T.Eof ] )
    ; ( "adjacent expressions"
      , "(\\x.x)(\\y.y)"
      , [ T.LParen
        ; T.Lambda
        ; T.Ident "x"
        ; T.Dot
        ; T.Ident "x"
        ; T.RParen
        ; T.LParen
        ; T.Lambda
        ; T.Ident "y"
        ; T.Dot
        ; T.Ident "y"
        ; T.RParen
        ; T.Eof
        ] )
    ; ( "definition"
      , "let id=\\x.x"
      , [ T.Let
        ; T.Ident "id"
        ; T.Equals
        ; T.Lambda
        ; T.Ident "x"
        ; T.Dot
        ; T.Ident "x"
        ; T.Eof
        ] )
    ; ( "mixed whitespace in abstraction"
      , " \t\\ x \n.\r\n x "
      , [ T.Lambda; T.Ident "x"; T.Dot; T.Ident "x"; T.Eof ] )
    ; ( "mixed whitespace in application"
      , "\n(\tfoo\r\nbar \t)\r"
      , [ T.LParen; T.Ident "foo"; T.Ident "bar"; T.RParen; T.Eof ] )
    ; ( "mixed whitespace in definition"
      , "\tlet id \r=\n λx.\t x\r\n"
      , [ T.Let
        ; T.Ident "id"
        ; T.Equals
        ; T.Lambda
        ; T.Ident "x"
        ; T.Dot
        ; T.Ident "x"
        ; T.Eof
        ] )
    ; "unicode token boundary", "xλy", [ T.Ident "x"; T.Lambda; T.Ident "y"; T.Eof ]
    ; "invalid character", "@", [ T.Invalid; T.Eof ]
    ; "invalid identifier start", "_x", [ T.Invalid; T.Ident "x"; T.Eof ]
    ]
  in
  "kinds", List.map make_test cases
;;

let () = Alcotest.run "lexer" [ kind_tests () ]
