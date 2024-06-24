val print_search : Geneweb.Config.config -> Gwdb.base -> unit

type dico = string array

val dico_fname :
  string ->
  string ->
  [< `area_code | `country | `county | `region | `subdivision | `town ] ->
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
