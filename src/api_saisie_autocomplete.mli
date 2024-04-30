val get_list_from_cache :
  ?retry:bool ->
  Geneweb.Config.config ->
  Gwdb.base ->
  [< `firstname | `lastname | `place | `source > `place ] ->
  int ->
  string ->
  string list
