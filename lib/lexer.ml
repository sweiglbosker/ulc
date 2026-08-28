module T = Token

type state =
  | Init
  | IdentOrKeyword

type t =
  { buf : string
  ; st : int
  ; pos : int
  ; state : state
  }

let make buf = { buf; st = 0; pos = 0; state = Init }

let peek ?(offset = 0) t =
  let pos = t.pos + offset in
  if pos >= String.length t.buf then None else Some t.buf.[pos]
;;

let consume ?(offset = 1) t = { t with pos = t.pos + offset }
let seek t newpos = { t with st = newpos; pos = newpos }
let is_whitespace char = char = ' ' || char = '\t' || char = '\n' || char = '\r'
let is_nonzero_digit char = char >= '1' && char <= '9'
let is_digit char = char = '0' || is_nonzero_digit char
let is_alpha char = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
let is_alnum char = is_alpha char || is_digit char

let starts_with_at source position prefix =
  let length = String.length prefix in
  position + length <= String.length source && String.sub source position length = prefix
;;

let rec next t =
  let tok ?(width = 1) kind =
    let ed = t.pos + width in
    let token = T.make t.st ~ed kind in
    token, { t with state = Init; st = ed; pos = ed }
  in
  match t.state with
  | Init ->
    (match peek t with
     | None -> tok ~width:0 T.Eof
     | Some x when is_whitespace x -> next (seek t (t.pos + 1))
     | Some '(' -> tok T.LParen
     | Some ')' -> tok T.RParen
     | Some '\\' -> tok T.Lambda
     | Some '=' -> tok T.Equals
     | Some '.' -> tok T.Dot
     | Some x when is_alpha x -> next { (consume t) with state = IdentOrKeyword }
     | Some _ when starts_with_at t.buf t.pos "λ" ->
       tok ~width:(String.length "λ") T.Lambda
     | Some _ -> tok T.Invalid)
  | IdentOrKeyword ->
    (match peek t with
     | Some c when is_alnum c || c = '_' || c = '\'' -> next (consume t)
     | _ ->
       let lexeme = String.sub t.buf t.st (t.pos - t.st) in
       (match T.from_keyword lexeme with
        | Some keyword -> tok ~width:0 keyword
        | None -> tok ~width:0 (T.Ident lexeme)))
;;

let rec scan' t tokens =
  let token, t' = next t in
  let tokens' = token :: tokens in
  match token.kind with
  | Eof -> List.rev tokens'
  | _ -> scan' t' tokens'
;;

let scan buf = scan' (make buf) []
