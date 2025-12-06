type operation =
  | Add
  | Mul

type task = {
  op : operation;
  values : int list;
}

let eval task = match task.op with
| Add -> List.fold_left ( + ) 0 task.values
| Mul -> List.fold_left ( * ) 1 task.values

let not_empty s = String.length s > 0

let split_on_elem elem li =
  let rec aux acc current = function
    | [] -> List.rev (current :: acc)
    | x :: xs -> if x = elem
      then aux (current :: acc) [] xs
      else aux acc (x :: current) xs
  in
  aux [] [] li
  |> List.map List.rev

let op_of_string s = match String.trim s with
| "*" -> Mul
| "+" -> Add
| _ -> failwith ("Unknown operation: " ^ s)

let parse_task = function
| o::[] -> failwith "No values provided"
| o::vs -> {op = op_of_string o; values = List.map int_of_string vs }
| _ -> failwith ("unknown pattern")

let rec transpose list = match list with
| [] -> []
| []::xss -> transpose xss
| (x::xs)::xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let explode s = List.init (String.length s) (String.get s)
let implode li = String.of_seq (List.to_seq li)

let task01 path =
    In_channel.with_open_text path In_channel.input_lines
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter not_empty)
    |> List.rev (* put operator to top *)
    |> transpose (* list of operator, value, value ...*)
    |> List.map parse_task
    |> List.map eval
    |> List.fold_left (+) 0
  
let task02 path =
  let content = In_channel.with_open_text path In_channel.input_lines
    |> List.rev in
  let ops = List.hd content
    |> String.split_on_char ' '
    |> List.filter not_empty
    |> List.map op_of_string
  in
  let values = List.tl content
    |> List.map explode
    |> List.rev
    |> transpose
    |> List.map implode
    |> List.map String.trim
    |> split_on_elem ""
    |> List.map (List.map int_of_string)
in
  List.map2 (fun o vs -> {op = o; values = vs}) ops values
  |> List.map eval
  |> List.fold_left (+) 0

let () =
  let t1 = task01 Sys.argv.(1) in
  let t2 = task02 Sys.argv.(1) in
  Printf.printf "%d\n%d\n" t1 t2
