module StrSet = Set.Make (String)

let sorted_array_of_set s =
  let a = StrSet.elements s |> Array.of_list in
  Array.sort Gutil.alphabetic a ;
  a

module type Store = sig
  type t
  val empty : t
end

module Make (Store : Store) = struct
  module Volume = struct
    type t = {
        name : string;
        add_entry : Store.t -> string list -> Store.t;
        get_all_entries : Store.t -> StrSet.t;
      }
  end

  type t = {
      csv_filename : string;
      volumes : Volume.t list;
    }

  let make ~csv_filename volumes = {csv_filename; volumes}

  let generate fname_set data =
    let ext_flags =
      [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
    in
    let oc = open_out_gen ext_flags 0o644 fname_set in
    output_value oc (data : Api_marshal_dico_place.dico) ;
    close_out oc

  let marshal ~assets ~lang {csv_filename = fname_csv; volumes} =
    Geneweb.GWPARAM.syslog `LOG_DEBUG ("writing files for lang "
                                       ^ lang ^ " from file: " ^ fname_csv);

    let csv = Api_csv.load_from_file ~file:fname_csv in

    let data = Store.empty in

    let data =
      Api_csv.fold_left
        (fun data row ->
          List.fold_left
            (fun data volume -> volume.Volume.add_entry data row) data volumes)
        data
        csv
    in
    List.iter
      (fun volume ->
        generate
          (Filename.concat
             assets (Printf.sprintf "dico.%s.%s.bin~" volume.Volume.name lang))
          (sorted_array_of_set (volume.Volume.get_all_entries data)))
      volumes

  let unmarshal_volume _ = failwith __LOC__
end
