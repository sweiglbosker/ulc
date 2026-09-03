type phrase =
  | Expression of Term.t
  | Definition of string * Term.t

type error =
  | Unexpected_token of
      { found : Token.t
      ; expected : string
      }

val parse_source : string -> (phrase, error) result
val pp_error : Format.formatter -> error -> unit
val pp_phrase : Format.formatter -> phrase -> unit
