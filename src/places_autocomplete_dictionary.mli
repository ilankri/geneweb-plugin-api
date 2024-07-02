val marshal : assets:string -> lang:string -> string -> unit

val unmarshal :
  assets:string ->
  lang:string ->
  [< Api_saisie_write_piqi.auto_complete_place_field ] ->
  string array
