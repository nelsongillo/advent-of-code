type tachyon = {
  location: int;
  count: int;
}

(* Merge duplicates from a list of tachyon beams *)
let merge li =
  let cmp a b = compare a.location b.location in
  let single_merge a b = match a with
  | [] -> [b]
  | x::xs -> if x.location = b.location
    then {location = b.location; count = x.count + b.count} :: xs
    else b::x::xs
  in 
  List.sort cmp li |> List.fold_left single_merge []

(* Extract the present values from an list of options *)
let get li = List.filter Option.is_some li |> List.map Option.get 

(* Explode a String into a char list *)
let explode s = List.init (String.length s) (String.get s)

(* get the starting position of the tachyon beam *)
let start grid = List.hd grid |> List.find_index ((==) 'S') |> Option.get

(* Split a beam *)
let split max beam =
  let left = if beam.location - 1 < 0 then None else Some {location = beam.location - 1; count = beam.count} in
  let right = if beam.location + 1 >= max then None else Some {location = beam.location + 1; count = beam.count} in
  get [left;right]

(* move the beam down the manifold by one *)
let beam_move (beams, acc) line =
  let rec aux (next, count) current =
    if List.nth line current.location == '^'
      then
        let splits = split (List.length line) current
        in
        (merge @@ splits @ next, count + 1)
      else (merge @@ current :: next, count)
  in
  List.fold_left aux ([], acc) beams
        
let task01 path =
    let content = In_channel.with_open_text path In_channel.input_lines |> List.map explode in
    let (_, count) = List.tl content |> List.fold_left beam_move ([{location=start content;count=1}], 0) in
    count
      
let task02 path =
    let content = In_channel.with_open_text path In_channel.input_lines |> List.map explode in
    let (beams, _) = List.tl content |> List.fold_left beam_move ([{location=start content;count=1}], 0) in
    List.fold_left (fun acc {count} -> acc + count) 0 beams


let () =
  let t1 = task01 Sys.argv.(1) in
  let t2 = task02 Sys.argv.(1) in
  Printf.printf "%d\n%d\n" t1 t2
