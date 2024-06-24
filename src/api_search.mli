val print_search : Geneweb.Config.config -> Gwdb.base -> unit

type dico = string array

val dico_fname :
  assets:string ->
  lang:string ->
  data_type:[< `area_code | `country | `county | `region | `subdivision | `town | `profession ] ->
  string option

val complete_with_dico :
  string ->
  Geneweb.Config.config ->
  int ref ->
  int ->
  [< `area_code
  | `country
  | `county
  | `region
  | `subdivision
  | `town ]
  option ->
  string ->
  string list ->
  string list
(** [complete_with_dico _ _ _ _ _ ini list]:
    [ini] must be in the form of [Name.lower @@ Mutil.tr '_' ' ' ini]
    Assume that [list] is already sorted, but reversed.
*)

val search_auto_complete :
  string ->
  Geneweb.Config.config ->
  Gwdb.base ->
  [< `firstname | `lastname | `place | `source ] ->
  [< `area_code
  | `country
  | `county
  | `region
  | `subdivision
  | `town ]
  option ->
  int ->
  string ->
  string list

val search_person_list :
  Gwdb.base -> string option -> string option -> Gwdb.iper list
