type t =
  | Var of string
  | Abs of string * t
  | App of t * t

module StringSet : Set.S with type elt = string
val free : t -> StringSet.t

val pp : Format.formatter -> t -> unit
val substitute : string -> t -> t -> t
