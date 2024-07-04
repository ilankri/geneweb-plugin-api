val dico_fname :
  string ->
  string ->
  [< Api_saisie_write_piqi.auto_complete_place_field ] ->
  string option

val write_dico_place_set : assets:string -> fname_csv:string -> lang:string -> unit
