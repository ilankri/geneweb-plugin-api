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

let generate fname_set data =
  let ext_flags =
    [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
  in
  let oc = open_out_gen ext_flags 0o644 fname_set in
  output_value oc (data : dico) ;
  close_out oc
