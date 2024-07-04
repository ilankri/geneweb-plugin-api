module StrSet = Set.Make (String)

let escape_dquote s =
  String.split_on_char '"' s
  |> String.concat "\\\""

let quote s =  "\"" ^ escape_dquote s ^ "\""

let build_line =
  let rec aux s l = match l with
    | [x] -> s ^ (quote x)
    | x :: xs -> aux (s ^ (quote x) ^ ",") xs
    | [] -> s
  in fun l -> aux "" l

let add_opt l set = match l with
  | x :: _ when x <> "" ->
     StrSet.add (build_line l) set
  | _ -> set

type dico = string array

let volume_filename ~assets ~lang name =
  Filename.concat assets (Printf.sprintf "dico.%s.%s.bin~" name lang)
