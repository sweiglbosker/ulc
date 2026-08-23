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

let rec fresh avoid binder =
  if not (StringSet.mem binder avoid) then binder else fresh avoid (binder ^ "'")
;;

let rec substitute variable replacement target =
  match target with
  | Var s -> if s = variable then replacement else Var s
  | App (l, r) ->
    App (substitute variable replacement l, substitute variable replacement r)
  | Abs (binder, body) ->
    if binder = variable
    then Abs (binder, body)
    else if not (StringSet.mem binder (free replacement))
    then Abs (binder, substitute variable replacement body)
    else (
      let binder' = fresh (StringSet.union (free body) (free replacement)) binder in
      Abs (binder', substitute variable replacement (substitute binder (Var binder') body)))
;;
