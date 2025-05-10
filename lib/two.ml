let rec last_two list =
  match list with
  | [] | [_] -> None
  | [x;y] -> Some (x,y)
  | _ :: tail -> last_two tail

let () =
 let list = [1; 2; 3; 4] in
  match last_two list with
  | None -> print_endline "The list is empty"
  | Some (x, y) -> print_endline ("The last elements are " ^ string_of_int x ^ string_of_int y)

let test_cases () =
  let _ = match last_two [] with
    | None -> print_endline "Empty list test passed"
    | Some (_, _) -> print_endline "Empty list test failed"
  in

  let _ = match last_two [1;2;3] with
      | Some (_, _) -> print_endline "Non Empty list test passed"
      | None -> print_endline "Non Empty list test failed"
  in
  ()

let () = test_cases ()
