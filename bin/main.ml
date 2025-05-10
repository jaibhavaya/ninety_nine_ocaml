(* 1. last item*)
let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

let () =
  let _ = match last [4;2] with
    | Some 2 -> print_endline "Last item test passed"
    | _ -> print_endline "Last item test failed"
  in
  ()

(* 2. last two items *)
let rec last_two list =
  match list with
  | [] | [_] -> None
  | [x;y] -> Some (x,y)
  | _ :: tail -> last_two tail

let () =
 let list = [1; 2; 3; 4] in
  match last_two list with
  | Some (3,4) -> print_endline "Last two items test passed"
  | _ -> print_endline "Last item test failed"

(* 3. item at k *)
let rec at k = function
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k-1) t

let () =
 let list = [1; 2; 3; 4] in
  match at 2 list with
      | Some 3 -> print_endline "Item at k test passed"
      | _ -> print_endline "Item at k  test failed"

