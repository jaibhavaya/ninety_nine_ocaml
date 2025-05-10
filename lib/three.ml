let rec at k = function
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k-1) t

let () =
 let list = [1; 2; 3; 4] in
  match at 2 list with
  | None -> print_endline "The list is empty"
  | Some x -> print_endline ("The element at k is " ^ string_of_int x)

let test_cases () =
  let _ = match at 1 [] with
    | None -> print_endline "Empty list test passed"
    | Some _ -> print_endline "Empty list test failed"
  in

  let _ = match at 1 [1;2;3] with
      | Some _ -> print_endline "Non Empty list test passed"
      | None -> print_endline "Non Empty list test failed"
  in
  ()

let () = test_cases ()
