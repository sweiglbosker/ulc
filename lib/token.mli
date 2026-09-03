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

val make : int -> ?ed:int -> kind -> t
val from_keyword : string -> kind option
val pp_kind : Format.formatter -> kind -> unit
