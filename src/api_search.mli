val print_search : Geneweb.Config.config -> Gwdb.base -> unit

val complete_with_dico :
  string ->
  Geneweb.Config.config ->
  int ref ->
  int ->
  [< Api_saisie_write_piqi.auto_complete_place_field ]
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
  [< Api_saisie_write_piqi.auto_complete_field ] ->
  [< Api_saisie_write_piqi.auto_complete_place_field ]
  option ->
  int ->
  string ->
  string list

val search_person_list :
  Gwdb.base -> string option -> string option -> Gwdb.iper list
