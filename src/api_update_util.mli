module Mwrite = Api_saisie_write_piqi

type created_person = {
  n : string;
  p : string;
  oc : int32
}

type update_base_status =
  | UpdateSuccess of
      Geneweb.CheckItem.base_warning list
      * Geneweb.CheckItem.base_misc list
      * (unit -> unit) list
      * created_person option
  | UpdateError of Geneweb.Update.update_error
  | UpdateErrorConflict of Api_saisie_write_piqi.Create_conflict.t

exception ModErrApiConflict of Api_saisie_write_piqi.Create_conflict.t

val created_person_of_person :
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  created_person

val created_person : n:string -> p:string -> oc:Int32.t -> created_person

val created_person_is_unnamed : created_person -> bool

val api_find_free_occ : Gwdb.base -> string -> string -> int

val check_person_conflict :
  Gwdb.base ->
  ( string * string * int * Geneweb.Update.create * 'a * bool,
    string )
  Def.gen_pers_event
  list ->
  ( Gwdb.iper,
    string * string * int * Geneweb.Update.create * 'a * bool,
    string )
  Def.gen_person ->
  unit

val check_family_conflict :
  Gwdb.base ->
  ( string * string * int * Geneweb.Update.create * _ * bool,
    _,
    _ )
  Def.gen_family ->
  (string * string * int * Geneweb.Update.create * _ * bool) Def.gen_couple ->
  (string * string * int * Geneweb.Update.create * _ * bool) Def.gen_descend ->
  unit

val date_of_piqi_date :
  Geneweb.Config.config -> Api_saisie_write_piqi.date -> Date.date option

val pers_to_piqi_person_search :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person_search

val pers_to_piqi_person_search_info :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person_search_info

val pers_to_piqi_person_link :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person_link

val pers_to_piqi_mod_person :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Api_saisie_write_piqi.person

val fam_to_piqi_mod_family :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.ifam ->
  Gwdb.family ->
  Api_saisie_write_piqi.family

val piqi_mod_person_of_person_start :
  Geneweb.Config.config ->
  Gwdb.base ->
  Api_piqi.person_start ->
  Api_saisie_write_piqi.person

val piqi_empty_family :
  Geneweb.Config.config ->
  Gwdb.base ->
  Gwdb.ifam ->
  Api_saisie_write_piqi.family

val reconstitute_somebody :
  Gwdb.base ->
  Api_saisie_write_piqi.person_link ->
  string * string * int * Geneweb.Update.create * string * bool
