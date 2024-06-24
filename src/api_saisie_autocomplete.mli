val get_list_from_cache :
  ?retry:bool ->
  Geneweb.Config.config ->
  Gwdb.base ->
  [< `firstname | `lastname | `place | `source ] ->
  int ->
  string ->
  string list
