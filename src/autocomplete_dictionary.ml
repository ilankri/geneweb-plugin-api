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

let sorted_array_of_set s =
  let a = StrSet.elements s |> Array.of_list in
  Array.sort Gutil.alphabetic a ;
  a

module type Volume = sig
  type t
  val make : process_csv_row:(string list -> string list) -> t
  val add_csv_row : string list -> t -> t
  val entries : t -> StrSet.t
end

module Volume : Volume = struct
  type t = {
      entries : StrSet.t;
      process_csv_row : string list -> string list;
    }

  let make ~process_csv_row = {entries = StrSet.empty; process_csv_row}

  let add_csv_row row volume =
    {volume with entries = add_opt (volume.process_csv_row row) volume.entries}

  let entries volume = volume.entries
end

let volume_filename ~assets ~lang name =
  Filename.concat assets (Printf.sprintf "dico.%s.%s.bin~" name lang)

type t = {
    csv_filename : string;
    volumes : (string * Volume.t) list;
  }

let make ~csv_filename volumes =
  let csv = Api_csv.load_from_file ~file:csv_filename in
  let volumes =
    Api_csv.fold_left
      (fun volumes row ->
        List.map
          (fun (name, volume) -> (name, Volume.add_csv_row row volume)) volumes)
      volumes
      csv
  in
  {csv_filename; volumes}

type dico = string array

let generate fname_set data =
  let ext_flags =
    [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
  in
  let oc = open_out_gen ext_flags 0o644 fname_set in
  output_value oc (data : dico) ;
  close_out oc

let marshal ~assets ~lang {csv_filename = fname_csv; volumes} =
  Geneweb.GWPARAM.syslog `LOG_DEBUG ("writing files for lang "
                                     ^ lang ^ " from file: " ^ fname_csv);
  List.iter
    (fun (name, volume) ->
      generate
        (volume_filename ~assets ~lang name)
        (sorted_array_of_set (Volume.entries volume)))
    volumes

let unmarshal_volume ~assets ~lang name =
  let unmarshal fn =
    Files.read_or_create_value fn (fun () : dico -> [||])
  in
  name
  |> volume_filename ~assets ~lang
  |> unmarshal
