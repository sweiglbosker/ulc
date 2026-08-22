type t =
  | Var of string
  | Abs of string * t
  | App of t * t

module StringSet = Set.Make (String)

let rec free = function
  | Var s -> StringSet.singleton s (* nothing is bound *)
  | Abs (x, t) -> StringSet.remove x (free t) (* remove the binding *)
  | App (t1, t2) -> StringSet.union (free t1) (free t2)
;;

(* in term t, replace every free occurence of the variable named "x" with the replacement term s. *)
let rec substitute variable replacement target =
  match target with
  | Var s -> if s = variable then replacement else Var s
  | App (l, r) ->
    App (substitute variable replacement l, substitute variable replacement r)
  | Abs (binder, body) -> (* TODO *)
;;
