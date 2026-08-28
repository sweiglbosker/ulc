type kind =
  | Lambda
  | Dot
  | LParen
  | RParen
  | Ident of string
  | Let
  | Equals
  | Invalid
  | Eof

type t =
  { kind : kind
  ; st : int
  ; ed : int
  }

let make st ?(ed = st + 1) kind = { kind; st; ed }

let from_keyword = function
  | "let" -> Some Let
  | _ -> None
;;

let pp_kind fmt =
  let p = Format.pp_print_string fmt in
  function
  | Lambda -> p "λ"
  | Dot -> p "."
  | LParen -> p "("
  | RParen -> p ")"
  | Ident s -> Format.fprintf fmt "Ident(%s)" s
  | Let -> p "let"
  | Equals -> p "="
  | Invalid -> p "<INVALID>"
  | Eof -> p "<EOF>"
;;
