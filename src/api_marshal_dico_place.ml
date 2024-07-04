let volume_name = function
  | `town -> Some "town"
  | `area_code -> Some "area_code"
  | `county -> Some "county"
  | `region -> Some "region"
  | `country -> Some "country"
  | `subdivision -> None

let dico_fname assets lang k =
  Option.map
    (Autocomplete_dictionary.volume_filename ~assets ~lang) (volume_name k)

let generate assets lang k data =
  match dico_fname assets lang k with
  | None -> ()
  | Some fname_set -> Autocomplete_dictionary.generate fname_set data

let sorted_array_of_set s =
  let a = Autocomplete_dictionary.StrSet.elements s |> Array.of_list in
  Array.sort Gutil.alphabetic a ;
  a

module PlacesData : sig
  type t
  val empty : t
  val add_town : t -> string list -> t
  val add_area_code : t -> string list -> t
  val add_county : t -> string list -> t
  val add_region : t -> string list -> t
  val add_country : t -> string list -> t
  val get_towns : t -> Autocomplete_dictionary.StrSet.t
  val get_area_codes : t -> Autocomplete_dictionary.StrSet.t
  val get_counties : t -> Autocomplete_dictionary.StrSet.t
  val get_regions : t -> Autocomplete_dictionary.StrSet.t
  val get_countries : t -> Autocomplete_dictionary.StrSet.t
end = struct
  type t = {
    towns : Autocomplete_dictionary.StrSet.t;
    area_codes : Autocomplete_dictionary.StrSet.t;
    counties : Autocomplete_dictionary.StrSet.t;
    regions : Autocomplete_dictionary.StrSet.t;
    countries : Autocomplete_dictionary.StrSet.t;
  }
  let empty = {
    towns = Autocomplete_dictionary.StrSet.empty;
    area_codes = Autocomplete_dictionary.StrSet.empty;
    counties = Autocomplete_dictionary.StrSet.empty;
    regions = Autocomplete_dictionary.StrSet.empty;
    countries = Autocomplete_dictionary.StrSet.empty;
  }
  let add_town t town =
    let towns = Autocomplete_dictionary.add_opt town t.towns in
    {t with towns}
  let add_area_code t area_code =
    let area_codes = Autocomplete_dictionary.add_opt area_code t.area_codes in
    {t with area_codes}
  let add_county t county =
    let counties = Autocomplete_dictionary.add_opt county t.counties in
    {t with counties}
  let add_region t region =
    let regions = Autocomplete_dictionary.add_opt region t.regions in
    {t with regions}
  let add_country t country =
    let countries = Autocomplete_dictionary.add_opt country t.countries in
    {t with countries}
  let get_towns t = t.towns
  let get_area_codes t = t.area_codes
  let get_counties t =  t.counties
  let get_regions t = t.regions
  let get_countries t = t.countries
end

let make ~fname_csv =
  let csv = Api_csv.load_from_file ~file:fname_csv in
  let data = PlacesData.empty in
  let log_malformed_line ~fname_csv l =
    Geneweb.GWPARAM.syslog `LOG_DEBUG ("malformed line in file: " ^ fname_csv);
    let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
    Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s)
  in
  Api_csv.fold_left (
      fun data ->
      function
      | [ town ; area_code ; county ; region ; country  ; country_code ] ->
         let data = PlacesData.add_town data [town; area_code; county; region; country; country_code] in
         let data = PlacesData.add_area_code data [area_code; county; region; country; country_code] in
         let data = PlacesData.add_county data [county; region; country; country_code] in
         let data = PlacesData.add_region data [region; country; country_code] in
         let data = PlacesData.add_country data [country; country_code] in
         data
      | l ->
         log_malformed_line ~fname_csv l;
         data
    ) data csv


let write_dico_place_set ~assets ~fname_csv ~lang =
  Geneweb.GWPARAM.syslog `LOG_DEBUG ("writing files for lang "
                                      ^ lang ^ " from file: " ^ fname_csv);
  let data = make ~fname_csv in
  let generate = generate assets lang in
  generate `town (sorted_array_of_set (PlacesData.get_towns data)) ;
  generate `area_code (sorted_array_of_set (PlacesData.get_area_codes data)) ;
  generate `county (sorted_array_of_set (PlacesData.get_counties data)) ;
  generate `region (sorted_array_of_set (PlacesData.get_regions data)) ;
  generate `country (sorted_array_of_set (PlacesData.get_countries data))
