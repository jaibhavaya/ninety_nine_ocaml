let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

let () =
 let list = [1; 2; 3; 4] in
  match last list with
  | None -> print_endline "The list is empty"
  | Some x -> print_endline ("The last element is " ^ string_of_int x)

let test_cases () =
  let _ = match last [] with
    | None -> print_endline "Empty list test passed"
    | Some _ -> print_endline "Empty list test failed"
  in

  let _ = match last [42] with
    | Some 42 -> print_endline "Single element test passed"
    | _ -> print_endline "Single element test failed"
  in

  let _ = match last [1; 2; 3; 4; 5] with
    | Some 5 -> print_endline "Multiple elements test passed"
    | _ -> print_endline "Multiple elements test failed"
  in
  ()

let () = test_cases ()
