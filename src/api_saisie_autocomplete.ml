(**/**)

module IstrSet = Set.Make (struct type t = Gwdb.istr let compare = compare end)

(** Create cache files  used by autocomplete *)
let create_cache base mode cache_file =
  let add acc x = if not (Gwdb.is_empty_string x) then IstrSet.add x acc else acc in
  let cache =
    match mode with
    | `lastname ->
      Gwdb.Collection.fold
        (fun acc p -> add acc (Gwdb.get_surname p) )
        IstrSet.empty
        (Gwdb.persons base)
    | `firstname ->
      Gwdb.Collection.fold
        (fun acc p -> add acc (Gwdb.get_first_name p) )
        IstrSet.empty (Gwdb.persons base)
    | `place ->
      let acc =
        Gwdb.Collection.fold
          (fun acc p ->
             List.fold_left
               (fun acc e -> add acc (Gwdb.get_pevent_place e)) acc (Gwdb.get_pevents p) )
          IstrSet.empty (Gwdb.persons base)
      in
      Gwdb.Collection.fold
        (fun acc f -> List.fold_left (fun acc e -> add acc (Gwdb.get_fevent_place e)) acc (Gwdb.get_fevents f) )
        acc (Gwdb.families base)
    | `source ->
      let acc =
        Gwdb.Collection.fold
          (fun acc p ->
             let acc = add acc (Gwdb.get_psources p) in
             List.fold_left (fun acc e -> add acc (Gwdb.get_pevent_src e)) acc (Gwdb.get_pevents p) )
          IstrSet.empty
          (Gwdb.persons base)
      in
      Gwdb.Collection.fold
        (fun acc f ->
           let acc = add acc (Gwdb.get_fsources f) in
           List.fold_left (fun acc e -> add acc (Gwdb.get_fevent_src e)) acc (Gwdb.get_fevents f) )
        acc
        (Gwdb.families base)
    | `occupation ->
       Gwdb.Collection.fold
         (fun occupations person ->
           add occupations (Gwdb.get_occupation person))
         IstrSet.empty (Gwdb.persons base)
  in
  let cache = List.rev_map (Gwdb.sou base) (IstrSet.elements cache) in
  let cache =
    List.sort
      (match mode with
       | `place -> Geneweb.Place.compare_places
       | `firstname | `lastname | `source | `occupation ->
          Gutil.alphabetic_order)
      cache
  in
  let oc = Secure.open_out_bin cache_file in
  Marshal.to_channel oc cache [ Marshal.No_sharing ] ;
  close_out oc

let rec get_list_from_cache ?(retry = true) conf base mode max_res s =
  let bfile = Geneweb.Util.bpath (conf.Geneweb.Config.bname ^ ".gwb") in
  let cache_file =
    match mode with
    | `lastname -> Filename.concat bfile "cache_surname"
    | `firstname -> Filename.concat bfile "cache_first_name"
    | `place -> Filename.concat bfile "cache_place"
    | `source -> Filename.concat bfile "cache_src"
    | `occupation -> Filename.concat bfile "cache_occupation"
  in
  Lock.control cache_file false ~onerror:(fun () -> []) begin fun () ->
    let stats = Unix.stat cache_file in
    let last_mod = conf.Geneweb.Config.ctime -. stats.Unix.st_mtime in
    if stats.Unix.st_size = 0 || last_mod > 3600.
    then create_cache base mode cache_file ;
    let ic = Secure.open_in_bin cache_file in
    try
      let cache : string list = Marshal.from_channel ic in
      let ini = Name.lower @@ Mutil.tr '_' ' ' s in
      (* optim : on sait que la liste est triée. *)
      let rec loop list accu nb_res =
        match list with
        | [] -> List.rev accu
        | name :: l ->
          let k = Mutil.tr '_' ' ' name in
          let (accu, nb_res) =
            if Mutil.start_with_wildcard ini 0 (Name.lower k)
            then name :: accu, nb_res + 1
            else accu, nb_res
          in
          if nb_res < max_res then loop l accu nb_res
          else List.rev accu
      in
      loop cache [] 0
    with
    |  _ when retry ->
      close_in ic ;
      Sys.remove cache_file ;
      get_list_from_cache ~retry:false conf base mode max_res s ;
    | e ->
      close_in ic ;
      raise e

  end
