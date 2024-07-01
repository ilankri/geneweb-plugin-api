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
  | `town > `subdivision ]
  option ->
  string ->
  string list ->
  string list

val search_auto_complete :
  string ->
  Geneweb.Config.config ->
  Gwdb.base ->
  [< `firstname | `lastname | `place | `source > `place `source ] ->
  [< `area_code
  | `country
  | `county
  | `region
  | `subdivision
  | `town > `subdivision ]
  option ->
  int ->
  string ->
  string list

val search_person_list :
  Gwdb.base -> string option -> string option -> Gwdb.iper list
