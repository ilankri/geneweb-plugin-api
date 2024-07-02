let make ~csv_filename =
  Autocomplete_dictionary.make
    ~csv_filename
    [ ("town",
       Autocomplete_dictionary.Volume.make
         ~process_csv_row:(function
           | [ town ; area_code ; county ; region ; country  ; country_code ] ->
              [town; area_code; county; region; country; country_code]
           | l ->
              Geneweb.GWPARAM.syslog
                `LOG_DEBUG ("malformed line in file: " ^ csv_filename);
              let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
              Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
              l));
      ("area_code",
       Autocomplete_dictionary.Volume.make
         ~process_csv_row:(function
           | [ _ ; area_code ; county ; region ; country  ; country_code ] ->
              [area_code; county; region; country; country_code]
           | l ->
              Geneweb.GWPARAM.syslog
                `LOG_DEBUG ("malformed line in file: " ^ csv_filename);
              let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
              Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
              l
         )
      );
      ("county",
       Autocomplete_dictionary.Volume.make
         ~process_csv_row:(function
           | [ _ ; _ ; county ; region ; country  ; country_code ] ->
              [county; region; country; country_code]
           | l ->
              Geneweb.GWPARAM.syslog
                `LOG_DEBUG ("malformed line in file: " ^ csv_filename);
              let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
              Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
              l
         )
      );
      ("region",
       Autocomplete_dictionary.Volume.make
         ~process_csv_row:(function
           | [ _ ; _ ; _ ; region ; country  ; country_code ] ->
              [region; country; country_code]
           | l ->
              Geneweb.GWPARAM.syslog
                `LOG_DEBUG ("malformed line in file: " ^ csv_filename);
              let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
              Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
              l
         )
      );
      ("country",
       Autocomplete_dictionary.Volume.make
         ~process_csv_row:(function
           | [ _ ; _ ; _ ; _ ; country  ; country_code ] ->
              [country; country_code]
           | l ->
              Geneweb.GWPARAM.syslog
                `LOG_DEBUG ("malformed line in file: " ^ csv_filename);
              let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
              Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
              l
         )
    ) ]

let volume_name = function
  | `town -> Some "town"
  | `area_code -> Some "area_code"
  | `county -> Some "county"
  | `region -> Some "region"
  | `country -> Some "country"
  | `subdivision -> None

let volume_filename ~assets ~lang place_field =
  Option.map
    (Autocomplete_dictionary.volume_filename ~assets ~lang)
    (volume_name place_field)

let marshal ~assets ~lang s =
  let e k =
    match volume_filename ~assets ~lang k with
    | None -> false
    | Some fn -> not (Sys.file_exists fn)
  in
  if e `town || e `area_code || e `county || e `region || e `country
  then Autocomplete_dictionary.marshal ~assets (make ~csv_filename:s) ~lang

let unmarshal ~assets ~lang mode =
  mode
  |> volume_name
  |> Option.map (Autocomplete_dictionary.unmarshal_volume ~assets ~lang)
  |> Option.value ~default:[||]
