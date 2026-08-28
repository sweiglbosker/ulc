let rec step =
  let is_value = function
    | Term.Abs _ -> true
    | _ -> false
  in
  function
  | Term.App (Term.Abs (binder, body), arg) ->
    if is_value arg
    then Some (Term.substitute binder arg body)
    else (
      match step arg with
      | Some arg' -> Some (Term.App (Term.Abs (binder, body), arg'))
      | None -> None)
  | Term.App (f, r) ->
    (match step f with
     | Some f' -> Some (Term.App (f', r))
     | None -> None)
  | _ -> None
;;

let rec eval expr =
  match step expr with
  | Some expr' -> eval expr'
  | None -> expr
;;
