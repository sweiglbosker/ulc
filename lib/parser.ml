type phrase =
  | Expression of Term.t
  | Definition of string * Term.t

type error =
  | Unexpected_token of
      { found : Token.t
      ; expected : string
      }

type t =
  { tokens : Token.t array
  ; mutable pos : int
  }

let make tokens =
  let length = Array.length tokens in
  assert (length > 0);
  assert (tokens.(length - 1).Token.kind = Token.Eof);
  { tokens; pos = 0 }
;;

let peek ?(offset = 0) parser =
  assert (offset >= 0);
  let last = Array.length parser.tokens - 1 in
  parser.tokens.(min (parser.pos + offset) last)
;;

let advance parser =
  let token = peek parser in
  if token.Token.kind <> Token.Eof then parser.pos <- parser.pos + 1;
  token
;;

let expect parser expected_kind =
  let found = peek parser in
  if found.Token.kind = expected_kind
  then Ok (advance parser)
  else (
    let expected = Format.asprintf "%a" Token.pp_kind expected_kind in
    Error (Unexpected_token { found; expected }))
;;

let expect_ident parser =
  let found = peek parser in
  match found.Token.kind with
  | Token.Ident name ->
    ignore (advance parser : Token.t);
    Ok name
  | _ -> Error (Unexpected_token { found; expected = "an identifier" })
;;

let ( let* ) = Result.bind

let rec parse_expression parser =
  match (peek parser).Token.kind with
  | Token.Lambda -> parse_abstraction parser
  | _ -> parse_application parser

and parse_abstraction parser =
  let* _ = expect parser Token.Lambda in
  let* binder = expect_ident parser in
  let* _ = expect parser Token.Dot in
  let* body = parse_expression parser in
  Ok (Term.Abs (binder, body))

and parse_application parser =
  let rec gather acc =
    match (peek parser).Token.kind with
    | Token.Ident _ | Token.LParen ->
      let* atom = parse_atom parser in
      gather (Term.App (acc, atom))
    | _ -> Ok acc
  in
  let* t1 = parse_atom parser in
  gather t1

and parse_atom parser =
  match (peek parser).Token.kind with
  | Token.LParen ->
    let* _ = expect parser Token.LParen in
    let* expression = parse_expression parser in
    let* _ = expect parser Token.RParen in
    Ok expression
  | _ ->
    let* name = expect_ident parser in
    Ok (Term.Var name)
;;

let parse_definition parser =
  let* _ = expect parser Token.Let in
  let* name = expect_ident parser in
  let* _ = expect parser Token.Equals in
  let* expression = parse_expression parser in
  Ok (Definition (name, expression))
;;

let parse parser =
  let* phrase =
    match (peek parser).Token.kind with
    | Token.Let -> parse_definition parser
    | _ ->
      let* expression = parse_expression parser in
      Ok (Expression expression)
  in
  let* _ = expect parser Token.Eof in
  Ok phrase
;;

let parse_source source =
  source |> Lexer.scan |> Array.of_list |> make |> parse

let pp_error fmt = function
  | Unexpected_token t ->
    Format.fprintf
      fmt
      "expected %s, found %a at %d-%d"
      t.expected
      Token.pp_kind
      t.found.kind
      t.found.st
      t.found.ed
;;

let pp_phrase fmt = function
  | Expression term -> Term.pp fmt term
  | Definition (name, term) -> Format.fprintf fmt "let %s = %a" name Term.pp term
;;
