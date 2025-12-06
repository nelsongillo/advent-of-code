(* --- Split the list at the first occurrence of elem --- *)
let split_at (elem : string) (lst : string list) =
  let rec aux acc = function
    | [] -> (List.rev acc, [])
    | x :: xs ->
      if String.equal x elem then
        (List.rev acc, xs)
      else
        aux (x :: acc) xs
  in
  aux [] lst

(* --- Parse a range "a-b" into (a, b) --- *)
let parse_fresh range =
  match String.split_on_char '-' range |> List.map int_of_string with
  | [a; b] -> (a, b)
  | _ -> failwith ("Invalid range: " ^ range)

let is_fresh value (a, b) = a <= value && value <= b

let count_fresh ingredients database =
  ingredients
  |> List.filter (fun i ->
         List.exists (is_fresh i) database)
  |> List.length

(* --- Merge two ranges assuming they are sorted by start --- *)
let merge acc (x1, x2) =
  match acc with
  | [] -> [ (x1, x2) ]
  | (a, b) :: rest ->
      if x1 > b then
        (* Disjoint, append new interval *)
        (x1, x2) :: acc
      else
        (* Overlapping or touching: merge *)
        (a, max b x2) :: rest

let get_count (a, b) = b - a + 1

let count_total (database : (int * int) list) =
  let sorted =
    List.sort (fun (a, _) (c, _) -> compare a c) database
  in
  let merged =
    List.fold_left merge [] sorted |> List.rev
  in
  merged
  |> List.map get_count
  |> List.fold_left ( + ) 0

(* --- Parse input file --- *)
let parse_file path =
  let fresh, all =
    In_channel.with_open_text path In_channel.input_lines
    |> split_at ""
  in
  let fresh_list = List.map parse_fresh fresh in
  let ingredients = List.map int_of_string all in
  (ingredients, fresh_list)

let () =
  let ingredients, database = parse_file Sys.argv.(1) in
  let fresh = count_fresh ingredients database in
  let count = count_total database in
  Printf.printf "%d\n%d\n" fresh count