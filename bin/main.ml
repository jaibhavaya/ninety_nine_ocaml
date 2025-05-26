(* 1. last item*)
let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

let () =
  let list = [4;2] in
  match last list with
    | Some 2 -> print_endline "Last item test passed"
    | _ -> print_endline "Last item test failed"

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
      | _ -> print_endline "Item at k test failed"

(* 4. length of list *)
let length list =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n+1) t
  in
  aux 0 list

let () =
 let list = [1; 2; 3; 4] in
  match length list with
      | 4 -> print_endline "Length of list test passed"
      | _ -> print_endline "Length of list test failed"

(* 5. reverse list *)
let reverse list =
  let rec aux output = function
  | [] -> output
  | x :: tail -> aux (x :: output) tail
  in
  aux [] list

let () =
 let list = [1; 2; 3; 4] in
  match reverse list with
      | [4;3;2;1] -> print_endline "Reverse list test passed"
      | _ -> print_endline "Reverse list test failed"

(* 6. palindrome *)
let palindrome list =
  list = reverse list

let () =
 let list = [1; 2; 3; 4] in
  match palindrome list with
      | false -> print_endline "Palindrome false test passed"
      | _ -> print_endline "Palindrome false test failed"

(* 7. run-length encoding *)
let encode list =
  let rec aux count output = function
    | [] -> []
    | [x] -> (count + 1, x) :: output
    | a :: ((b :: _) as t) -> if a = b then aux (count + 1) output t
                              else aux 0 ((count + 1, a) :: output) t in

  reverse (aux 0 [] list)

let () =
 let list = ["a"; "a"; "b"; "c"; "d"; "d"; "d"] in
 let expected = [(2, "a"); (1, "b"); (1, "c"); (3, "d")] in
  match encode list with
      | result when result = expected -> print_endline "Run-Length Encode test passed"
      | _ -> print_endline "Run-Length Encode test failed"


(* 8. modified run-length encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let modified_encode list =
  let create_tuple cnt elem =
    if cnt = 1 then One elem
    else Many (cnt, elem) in
  let rec aux count output = function
    | [] -> []
    | [x] -> (create_tuple (count + 1) x) :: output
    | a :: ((b :: _) as t) -> if a = b then aux (count + 1) output t
                              else aux 0 ((create_tuple (count +1) a) :: output) t in

  reverse (aux 0 [] list)

let () =
 let list = ["a"; "a"; "b"; "c"; "d"; "d"; "d"] in
 let expected = [Many (2, "a"); One "b"; One "c"; Many (3, "d")] in
  match modified_encode list with
      | result when result = expected -> print_endline "Run-Length Encode test passed"
      | _ -> print_endline "Run-Length Encode test failed"


(* 9. decode run-length encoding *)
let decode list =
  let rec many acc n e =
    if n = 0 then acc else many (e :: acc) (n-1) e
  in
  let rec aux acc = function
    | [] -> acc
    | One e :: t -> aux (e :: acc) t
    | Many(n, e) :: t -> aux (many acc n e) t
  in
  aux [] (reverse list)


