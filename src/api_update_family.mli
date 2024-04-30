val print_add :
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_saisie_write_piqi.family ->
  Api_saisie_write_piqi.person ->
  Api_saisie_write_piqi.person ->
  Gwdb.ifam option * Api_update_util.update_base_status

val print_mod :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  Api_saisie_write_piqi.family ->
  Api_update_util.update_base_status
