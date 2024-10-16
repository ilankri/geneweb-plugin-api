module M = Api_piqi
module Mext = Api_piqi_ext

(* Dans un premier temps, ce module dupliques certaines  *)
(* fonctions déjà présentes, mais c'est pour qu'il reste *)
(* le plus indépendant possible des autres modules.      *)

open Geneweb
open Config
open Def
open Gwdb
open Util
open Api_def

(* Convert safe_string to string *)
let (!!) = Adef.as_string

(* ... utils ... *)

let p_getenvbin = Api_piqi_util.p_getenvbin
let get_params = Api_piqi_util.get_params

let is_empty_or_quest_name p =
  is_empty_string (get_surname p) || is_quest_string (get_surname p) ||
  is_empty_string (get_first_name p) || is_quest_string (get_first_name p)

let get_portrait conf base p = Image.get_portrait conf base p |> Option.map Image.src_to_string

(**/**)


(* ********************************************************************* *)
(*  [Fonc] has_bas_loop : config -> base -> bool                         *)
(** [Description] : Renvoie true s'il y a une boucle dans la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - bool : Vrai s'il y a une boucle.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let has_base_loop conf base =
  try let _ = (Util.create_topological_sort conf base) in false
  with (Consang.TopologicalSortError _) -> true

let has_sosa_ref conf base =
  Util.find_sosa_ref conf base <> None

let compute_sosa conf base single_sosa =
  if not (has_sosa_ref conf base) then fun _ -> Sosa.zero
  else if has_base_loop conf base then fun _ -> Sosa.zero
  else if not single_sosa then
    let () = SosaCache.build_sosa_ht conf base in
    SosaCache.get_sosa_person
  else (SosaCache.get_single_sosa conf base)

(* Pour aller plus vite et ne pas tester l'existance de fichier    *)
(* plusieurs fois en fonction des extensions, on prend le problème *)
(* à l'envers et on charge tous les fichiers qui existe. Ensuite,  *)
(* on teste l'existence avec une Hashtbl.                          *)
let ht_img = Hashtbl.create 5003

let load_image_ht conf =
  let dir_img = base_path ["images"] conf.bname in
  let images =
    if Sys.file_exists dir_img then Sys.readdir dir_img
    else [||]
  in
  Array.iter
    (fun img ->
      if img = "old" then ()
      else
        try
          let name = Filename.chop_extension img in
          Hashtbl.add ht_img name img
(*
          let i = String.rindex name '.' in
          let key =
            String.sub name 0 i ^ " " ^
              String.sub name (i+1) (String.length name - i - 1)
          in
          (* Que c'est long !!! *)
          match Gutil.person_ht_find_all base key with
          | [] -> ()
          | [ip] -> Hashtbl.add ht_img ip img
          | l ->
              let rec loop l =
                match l with
                | [] -> ()
                | ip :: l ->
                    let p = poi base ip in
                    if Util.default_image_name base p = img then
                      Hashtbl.add ht_img ip img
                    else
                      loop l
              in loop l
*)
        with _ -> ())
    images

let find_image_ht name = try Hashtbl.find ht_img name with Not_found -> ""

(* BIENTOT DEPRECATED *)
let string_of_prec_dmy d =
  let s =
    if d.month = 0 then string_of_int d.year
    else if d.day = 0 then string_of_int d.month ^ "/" ^ string_of_int d.year
    else string_of_int d.day ^ "/" ^ string_of_int d.month ^ "/" ^ string_of_int d.year
  in
  match d.prec with
   | Sure -> Mutil.nominative s
   | About -> "~" ^ s
   | Before -> "<" ^ s
   | After -> ">" ^ s
   | Maybe -> "?" ^ s
   | OrYear d2 -> s ^ "|" ^ string_of_int d2.year2
   | YearInt d2 -> s ^ ".." ^ string_of_int d2.year2

let string_of_date = function
    Dgreg (d, _) -> string_of_prec_dmy d
  | Dtext t -> "(" ^ t ^ ")"


(* Lecture et écriture des dates, directement empruntées à gwcomp/gwu *)

let string_of_dmy d =
  let soy y = if y = 0 then "-0" else string_of_int y in
  let prec =
    match d.prec with
    | About -> "~"
    | Maybe -> "?"
    | Before -> "<"
    | After -> ">"
    | _ -> ""
  in
  let date =
    if d.month = 0 then soy d.year
    else if d.day = 0 then string_of_int d.month ^ "/" ^ soy d.year
    else string_of_int d.day ^ "/" ^ string_of_int d.month ^ "/" ^ soy d.year
  in
  let delta =
    match d.prec with
    | OrYear d2 -> "|" ^ soy d2.year2
    | YearInt d2 -> ".." ^ soy d2.year2
    | _ -> ""
  in
  prec ^ date ^ delta


(* ********************************************************************* *)
(*  [Fonc] string_of_date2 : string -> Def.date option                   *)
(** [Description] : Renvoie la string d'une date. Directement emprunté
                    de gwu.
    [Args] :
      - date : date convertir en string
    [Retour] :
      - string : renvoie une date au format GeneWeb.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let string_of_date2 date =
  let spaces_to_underscore s =
    String.init (String.length s)
      (fun i ->
         match s.[i] with
         | ' ' -> '_'
         | x -> x)
  in
  match date with
  | Dgreg (d, Dgregorian) -> string_of_dmy d
  | Dgreg (d, Djulian) -> string_of_dmy (Calendar.julian_of_gregorian d) ^ "J"
  | Dgreg (d, Dfrench) -> string_of_dmy (Calendar.french_of_gregorian d) ^ "F"
  | Dgreg (d, Dhebrew) -> string_of_dmy (Calendar.hebrew_of_gregorian d) ^ "H"
  | Dtext t -> Printf.sprintf "0(%s)" (spaces_to_underscore t)


let string_of_date_option date =
  match date with
  | Some d -> string_of_date2 d
  | None -> ""

let title_to_piqi_title t =
  let (title_type, name) =
    match t.t_name with
    | Tmain -> (`title_main, "")
    | Tname name -> (`title_name, name)
    | Tnone -> (`title_none, "")
  in
  let title = t.t_ident in
  let fief = t.t_place in
  let date_begin =
    match Date.od_of_cdate t.t_date_start with
    | Some d -> Some (string_of_date d)
    | None -> None
  in
  let date_end =
    match Date.od_of_cdate t.t_date_end with
    | Some d -> Some (string_of_date d)
    | None -> None
  in
  let nth = Some (Int32.of_int t.t_nth) in
  M.Title.{ title_type
          ; name = if name = "" then None else Some name
          ; title = if title = "" then None else Some title
          ; fief = if fief = "" then None else Some fief
          ; date_begin
          ; date_end
          ; nth
          }


(**/**) (* Convertion d'une date. *)

module Date_converter
    (M : sig
       module Dmy : sig
         type t = { mutable day : int32
                  ; mutable month : int32
                  ; mutable year : int32
                  ; mutable delta : int32
                  }
       end
       module Date : sig
         type t = { mutable cal : [ `gregorian | `julian | `french | `hebrew ] option
                  ; mutable prec : [ `sure | `about | `maybe | `before | `after | `oryear | `yearint ] option
                  ; mutable dmy : Dmy.t option
                  ; mutable dmy2 : Dmy.t option
                  ; mutable text : string option
                  }
       end
     end) =
struct
  let piqi_date_of_date = function
    | Dgreg (dmy, cal) ->
      let cal =
        match cal with
        | Dgregorian -> `gregorian
        | Djulian -> `julian
        | Dfrench -> `french
        | Dhebrew -> `hebrew
      in
      let (prec, dmy, dmy2) =
        let (d, m, y, delta) =
          (Int32.of_int dmy.day, Int32.of_int dmy.month,
           Int32.of_int dmy.year, Int32.of_int dmy.delta)
        in
        let dmy1 = {M.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear d2 ->
            let dmy2 =
              {
                M.Dmy.day = Int32.of_int 0;
                month = Int32.of_int 0;
                year = Int32.of_int d2.year2;
                delta = Int32.of_int 0;
              }
            in
            (`oryear, Some dmy2)
          | YearInt d2 ->
            let dmy2 =
              {
                M.Dmy.day = Int32.of_int 0;
                month = Int32.of_int 0;
                year = Int32.of_int d2.year2;
                delta = Int32.of_int 0;
              }
            in
            (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        M.Date.cal = Some cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
    | Dtext txt ->
      {
        M.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }

  let date_of_piqi_date date =
    match date.M.Date.text with
    | Some txt -> Dtext txt
    | _ ->
      let cal =
        match date.M.Date.cal with
        | Some `julian -> Djulian
        | Some `french -> Dfrench
        | Some `hebrew -> Dhebrew
        | _ -> Dgregorian
      in
      let prec =
        match date.M.Date.prec with
        | Some `about -> About
        | Some `maybe -> Maybe
        | Some `before -> Before
        | Some `after -> After
        | Some `oryear ->
          (match date.M.Date.dmy2 with
           | Some dmy ->
             let y = Int32.to_int dmy.M.Dmy.year in
             let dmy2 = {day2 = 0; month2 = 0; year2 = y; delta2 = 0} in
             OrYear dmy2
           | None -> OrYear {day2 = 0; month2 = 0; year2 = 0; delta2 = 0} (* erreur*))
        | Some `yearint ->
          (match date.M.Date.dmy2 with
           | Some dmy ->
             let y = Int32.to_int dmy.M.Dmy.year in
             let dmy2 = {day2 = 0; month2 = 0; year2 = y; delta2 = 0} in
             YearInt dmy2
           | None -> YearInt {day2 = 0; month2 = 0; year2 = 0; delta2 = 0} (* erreur*))
        | _ -> Sure
      in
      let dmy =
        match date.M.Date.dmy with
        | Some dmy ->
          let day = Int32.to_int dmy.M.Dmy.day in
          let month = Int32.to_int dmy.M.Dmy.month in
          let year = Int32.to_int dmy.M.Dmy.year in
          let delta = Int32.to_int dmy.M.Dmy.delta in
          {day = day; month = month; year = year; prec = prec; delta = delta}
        | None -> (* erreur*)
          {day = 0; month = 0; year = 0; prec = Sure; delta = 0}
      in
      Dgreg (dmy, cal)

end

include Date_converter (M)

let p_publicname base p =
  let public_name = Mutil.nominative (sou base (get_public_name p)) in
  if public_name = "" then None
  else Some public_name

let parent_has_title conf base p =
  match get_parents p with
  | Some ifam ->
      let cpl = foi base ifam in
      let fath = pget conf base (get_father cpl) in
      let moth = pget conf base (get_mother cpl) in
      get_access fath <> Private && nobtit conf base fath <> [] ||
      get_access moth <> Private && nobtit conf base moth <> []
  | _ -> false


(* ********************************************************************* *)
(*  [Fonc] date_included : dmy -> dmy -> dmy -> bool                     *)
(** [Description] : d1 <= d <= d2
    [Args] :
      - d  : date
      - d1 : date min
      - d2 : date max
    [Retour] :
      - bool : renvoie d1 <= d <= d2.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let date_included d d1 d2 =
  (* Fonction générique de test: y <= x <= z *)
  (* Le paramètre max permet de tester par   *)
  (* rapport au nombre max de jour ou mois.  *)
  let comp x y z max =
    if y <= z then (y <= x) && (x <= z)
    else if max > 0 then ((y <= x) && (x <= max)) || ((1 <= x) && (x <= z))
    else false
  in
  let (d, m, y) = (d.day, d.month, d.year) in
  let { day = d2 ; month = m2 ; year = y2 } = d2 in
  match d1 with
  | {day = 0; month = 0; year = 0} -> false
  | {day = d1; month = 0; year = 0} ->
    d2 <> 0 && m2 = 0 && y2 = 0 && d > 0 && comp d d1 d2 31
  | {day = 0; month = m1; year = 0} ->
    m2 <> 0 && d2 = 0 && y2 = 0 && m > 0 && comp m m1 m2 12
  | {day = 0; month = 0; year = y1} ->
    y2 <> 0 && d2 = 0 && m2 = 0 && comp y y1 y2 0
  (* Impossible pour GeneWeb *)
  | {day = d1; month = m1; year = 0} ->
    d2 <> 0 && m2 <> 0 && y2 = 0
    && d > 0
    && m > 0
    && comp (m * 100 + d) (m1 * 100 + d1) (m2 * 100 + d2) (12 * 100 + 31)
  | {day = 0; month = m1; year = y1} ->
    m2 <> 0 && y2 <> 0 && d2 = 0
    && m > 0 && comp (y * 100 + m) (y1 * 100 + m1) (y2 * 100 + m2) 0
  (* Impossible pour GeneWeb *)
  | {day = d1; month = 0; year = y1} ->
    d2 <> 0 && y2 <> 0 && m2 = 0
    && d > 0 && y1 = y2 && comp d d1 d2 31
  | {day = d1; month = m1; year = y1} ->
    y2 <> 0 &&
    comp
      (y * 10000 + m * 100 + d)
      (y1 * 10000 + m1 * 100 + d1)
      (y2 * 10000 + m2 * 100 + d2)
      0

(**/**) (* Divers filtres possibles. *)

(* ********************************************************************* *)
(*  [Fonc] reduce_to_sosa : person list -> person list                   *)
(** [Description] : Renvoie la liste des personnes ayant un numéro sosa.
    [Args] :
      - l    : liste de personnes
    [Retour] :
      - person list : Retourne la liste des personnes avec un sosa.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let reduce_to_sosa compute_sosa l =
  let rec loop l accu =
    match l with
    | [] -> accu
    | p :: l ->
        let sosa = compute_sosa p in
        if Sosa.gt sosa Sosa.zero then loop l (p :: accu)
        else loop l accu
  in loop l []


(* ********************************************************************* *)
(*  [Fonc] reduce_to_recent : config -> person list -> person list       *)
(** [Description] : Renvoie la liste des contemporains.
    [Args] :
      - conf : configuration de la base
      - l    : liste de personnes
    [Retour] :
      - person list : Retourne la liste des contemporains.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let reduce_to_recent conf l =
  let tmp_conf = {(conf) with private_years = max 85 conf.private_years} in
  let rec loop l accu =
    match l with
    | [] -> accu
    | p :: l ->
        if Util.is_old_person tmp_conf (gen_person_of_person p) then
          loop l accu
        else
          loop l (p :: accu)
  in loop l []


(* *********************************************************************** *)
(*  [Fonc] is_visible : config -> base -> person -> bool                   *)
(** [Description] : Renvoie vrai si l'on peut afficher les informations
                    d'une personne. Une personne est visible si elle n'est
                    pas privée OU si elle n'est plus contemporaine.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                            *)
(* *********************************************************************** *)
let is_visible conf base p =
  let tmp_conf = {(conf) with wizard = false; friend = false} in
  Util.authorized_age tmp_conf base p


(* ********************************************************************* *)
(*  [Fonc] is_sosa : (person -> Sosa.t) -> person -> bool            *)
(** [Description] : Test si la personne est un sosa.
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_sosa compute_sosa p =
  Sosa.gt (compute_sosa p) Sosa.zero


(* ********************************************************************* *)
(*  [Fonc] is_recent : config -> person -> bool                          *)
(** [Description] : Test si la personne est un contemporain.
    [Args] :
      - conf : configuration de la base
      - p    : person
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_recent conf p =
  let tmp_conf =
    {(conf) with private_years = max 85 conf.private_years;
      (* !!! Si on n'a pas de dates, on considère qu'on est contemporain.
         (Mantis 1327) *)
      public_if_no_date = false}
  in
  not (Util.is_old_person tmp_conf (gen_person_of_person p))


(* ********************************************************************* *)
(*  [Fonc] check_sex : person -> Def.sex -> bool                         *)
(** [Description] : Test si la personne est du même sexe que sex.
    [Args] :
      - conf : configuration de la base
      - p    : person
      - sex  : sexe que l'on cherche
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let check_sex p sex = get_sex p = sex


(* ********************************************************************* *)
(*  [Fonc] is_date_included : bool -> date -> date -> date -> bool       *)
(** [Description] : Test si d1 <= d <= d2.
    [Args] :
      - prec   : booléen pour savoir si l'on veut tester une date précise
                 (par exemple Octobre 1800 n'est pas une date précise)
      - d      : date que l'on cherche
      - d1, d2 : interval de date
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_date_included prec d d1 d2 =
  match d with
  | Some (Dgreg (d, _)) ->
      ((prec && d.prec = Sure) || not prec) && date_included d d1 d2
  | _ -> false


(* ********************************************************************* *)
(*  [Fonc] apply_filters_p : config -> filters -> person -> bool *)
(** [Description] : Test en fonction des filtres défini si la personne
                    répond aux critères (true) ou pas (false).
    [Args] :
      - conf    : configuration de la base
      - filters : filtres demandés
      - p       : person
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let apply_filters_p conf filters compute_sosa p =
  let filter = true in
  let filter =
    if filter then
      match filters.filter_sex with
      | Some sex -> check_sex p sex
      | None -> filter
    else filter
  in
  let filter =
    if filter && filters.only_sosa then is_sosa compute_sosa p
    else filter
  in
  let filter =
    if filter && filters.only_recent then is_recent conf p
    else filter
  in
  let filter =
    if filter then
      match filters.date_birth with
      | Some (date_begin, date_end, prec) ->
          is_date_included
            prec (Date.od_of_cdate (get_birth p)) date_begin date_end
      | None -> filter
    else filter
  in
  if filter then
    match filters.date_death with
    | Some (date_begin, date_end, prec) ->
      let death =
        match get_death p with
        | Death (_, cd) -> Some (Date.date_of_cdate cd)
        | _ -> None
      in
      is_date_included prec death date_begin date_end
    | None -> filter
  else filter


(**/**) (* Fonctions IO *)

module Filter = Api_piqi_util.Filter (M) (Mext)

module ReferencePerson = Api_piqi_util.ReferencePerson (M)

let person_to_reference_person = ReferencePerson.person_to_reference_person

let empty_reference_person = ReferencePerson.empty_reference_person

let get_filters = Filter.get_filters

let print_result = Api_piqi_util.print_result

let date_to_opt_string d =
  match Date.od_of_cdate d with
  | Some d -> Some (string_of_date d)
  | _ -> None


let person_to_warning_person base p =
  let iper = Gwdb.string_of_iper (get_iper p) in
  let lastname = sou base (get_surname p) in
  let firstname = sou base (get_first_name p) in
  let birth_date = date_to_opt_string @@ get_birth p in
  let death_date =
    match get_death p with
    | Death (_, d) -> date_to_opt_string d
    | _ -> None
  in
  let oc = Int32.of_int (get_occ p) in
  let n = Name.lower lastname in
  let p = Name.lower firstname in
  { M.Warning_person.n
  ; p
  ; oc
  ; firstname
  ; lastname
  ; birth_date
  ; death_date
  ; iper
  }

(**/**) (* Fonctions de transformation person <=> piqi person *)

let piqi_ref_person_to_person base ref_person =
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
  | Some ip -> Some (poi base ip)
  | None -> None


(* ********************************************************************* *)
(*  [Fonc] empty_piqi_person_light : Reference_person -> Person          *)
(** [Description] : Retourne à partir d'une Reference_person, une Person
                    dont tous les champs sont "vide" sauf (n, p, oc).
    [Args] :
      - ref_person : Reference_person
    [Retour] :
      - Person : Retourne une personne "vide".
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let empty_piqi_person_light conf ref_person =
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  {
    M.Person.sosa = "0";
    n = sn;
    p = fn;
    oc = occ;
    index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
    sex = `unknown;
    lastname = "";
    firstname = "";
    public_name = None;
    image = "";
    birth_date = "";
    birth_place = "";
    baptism_date = "";
    baptism_place = "";
    death_date = "";
    death_place = "";
    death_type = `not_dead;
    burial_date = "";
    burial_place = "";
    spouses = [];
    ascend = false;
    descend = false;
    visible_for_visitors = false;
    baseprefix = conf.command;
  }


(* ********************************************************************* *)
(*  [Fonc] empty_piqi_person_full : Reference_person -> Person           *)
(** [Description] : Retourne à partir d'une Reference_person, une Person
                    dont tous les champs sont "vide" sauf (n, p, oc).
    [Args] :
      - ref_person : Reference_person
    [Retour] :
      - Person : Retourne une personne "vide".
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let empty_piqi_person_full conf ref_person =
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  {
    M.Full_person.sosa = "0";
    n = sn;
    p = fn;
    oc = occ;
    index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
    sex = `unknown;
    lastname = "";
    firstname = "";
    public_name = None;
    aliases = [];
    qualifiers = [];
    firstname_aliases = [];
    surname_aliases = [];
    image = None;
    birth_date = None;
    birth_place = None;
    birth_src = None;
    baptism_date = None;
    baptism_place = None;
    baptism_src = None;
    death_date = None;
    death_place = None;
    death_src = None;
    death_type = `not_dead;
    burial_date = None;
    burial_place = None;
    burial_src = None;
    occupation = None;
    psources = None;
    titles = [];
    related = [];
    rparents = [];
    visible_for_visitors = false;
    parents = None;
    families = [];
    baseprefix = conf.command;
  }


let empty_piqi_person conf ref_person =
  if p_getenvbin conf.env "full_infos" = Some "1"
  then PFull (empty_piqi_person_full conf ref_person)
  else PLight (empty_piqi_person_light conf ref_person)


(* ************************************************************************** *)
(*  [Fonc] spouse_to_piqi_spouse :
             config -> base -> person -> family -> bool ->
             (person -> Sosa.t) -> Perso                                      *)
(** [Description] : Retourne à partir d'une person (gwdb) une Spouse (piqi)
                    dont tous les champs sont complétés.
                    Les tests de droits d'accès sont fait dans cette fonction.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
      - fam       : family
      - base_loop : booléen pour savoir s'il y a une boucle dans la base.
      - compute_sosa : appel de soit SosaCache.get_single_sosa,
                                soit SosaCache.get_sosa_person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let spouse_to_piqi_spouse conf base p fam compute_sosa =
  let gen_p = Util.string_gen_person base (gen_person_of_person p) in
  let p_auth = authorized_age conf base p in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  let sosa_p = Sosa.to_string (compute_sosa p) in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname =
    if not p_auth && (is_hide_names conf p) then ""
    else gen_p.surname
  in
  let first_name =
    if not p_auth && (is_hide_names conf p) then ""
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (get_occ p) in
  let publicname = if gen_p.public_name = "" then None else Some gen_p.public_name in
  let image =
    match Image.get_portrait conf base p with
    | Some src ->
        if gen_p.image <> "" then gen_p.image else Image.src_to_string src
    | None -> ""
  in
  let birth =
    match Date.od_of_cdate gen_p.birth with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let birth_place =
    if p_auth then gen_p.birth_place
    else ""
  in
  let baptism =
    match Date.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let baptism_place =
    if p_auth then gen_p.baptism_place
    else ""
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, "")
      | Death (_, cd) ->
          let d = Date.date_of_cdate cd in
          (`dead, string_of_date d)
      | DeadYoung -> (`dead_young, "")
      | DeadDontKnowWhen -> (`dead_dont_know_when, "")
      | DontKnowIfDead -> (`dont_know_if_dead, "")
      | OfCourseDead -> (`of_course_dead, "")
    else
      (`not_dead, "")
  in
  let death_place =
    if p_auth then gen_p.death_place
    else ""
  in
  let burial =
    match gen_p.burial with
    | Buried cod | Cremated cod ->
        (match Date.od_of_cdate cod with
        | Some d when p_auth -> string_of_date d
        | _ -> "")
    | _ -> ""
  in
  let burial_place =
    if p_auth then gen_p.death_place
    else ""
  in
  let marriage_date =
    match Date.od_of_cdate (get_marriage fam) with
    | Some d when m_auth -> string_of_date d
    | _ -> ""
  in
  let marriage_place =
    if m_auth then sou base (get_marriage_place fam)
    else ""
  in
  let divorce_type =
    if m_auth then
      match get_divorce fam with
      | NotDivorced -> `not_divorced
      | Divorced _ -> `divorced
      | Separated -> `separated
    else `not_divorced
  in
  let visible = is_visible conf base p in
  {
    M.Spouse.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    image = image;
    birth_date = birth;
    birth_place = birth_place;
    baptism_date = baptism;
    baptism_place = baptism_place;
    death_date = death;
    death_place = death_place;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    marriage_date = marriage_date;
    marriage_place = marriage_place;
    divorce_type = divorce_type;
    visible_for_visitors = visible;
  }


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_light :
             config -> base -> person -> bool -> (person -> Sosa.t) -> Person *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person (piqi)
                    (piqi) dont tous les champs sont complétés.
                    Les tests de droits d'accès sont fait dans cette fonction.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
      - base_loop : booléen pour savoir s'il y a une boucle dans la base.
      - compute_sosa : appel de soit SosaCache.get_single_sosa,
                                soit SosaCache.get_sosa_person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_light conf base p compute_sosa =
  let gen_p = Util.string_gen_person base (gen_person_of_person p) in
  let p_auth = authorized_age conf base p in
  let sosa_p = Sosa.to_string (compute_sosa p) in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname =
    if not p_auth && (is_hide_names conf p) then ""
    else gen_p.surname
  in
  let first_name =
    if not p_auth && (is_hide_names conf p) then ""
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (get_occ p) in
  let publicname = if gen_p.public_name = "" then None else Some gen_p.public_name in
  let image =
    match Image.get_portrait conf base p with
    | Some src ->
        if gen_p.image <> "" then gen_p.image else Image.src_to_string src
    | None -> ""
  in
  let birth =
    match Date.od_of_cdate gen_p.birth with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let birth_place =
    if p_auth then gen_p.birth_place
    else ""
  in
  let baptism =
    match Date.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let baptism_place =
    if p_auth then gen_p.baptism_place
    else ""
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, "")
      | Death (_, cd) ->
          let d = Date.date_of_cdate cd in
          (`dead, string_of_date d)
      | DeadYoung -> (`dead_young, "")
      | DeadDontKnowWhen -> (`dead_dont_know_when, "")
      | DontKnowIfDead -> (`dont_know_if_dead, "")
      | OfCourseDead -> (`of_course_dead, "")
    else
      (`not_dead, "")
  in
  let death_place =
    if p_auth then sou base (get_death_place p)
    else ""
  in
  let burial =
    match gen_p.burial with
    | Buried cod | Cremated cod ->
        (match Date.od_of_cdate cod with
        | Some d when p_auth -> string_of_date d
        | _ -> "")
    | _ -> ""
  in
  let burial_place =
    if p_auth then gen_p.burial_place
    else ""
  in
  let faml = Array.to_list (get_family p) in
  let sl =
    List.map
      (fun ifam ->
        let fam = foi base ifam in
        let c = Gutil.spouse (get_iper p) fam in
        (pget conf base c, fam) )
      faml
  in
  let sl =
    List.map
      (fun (p, fam) ->
        spouse_to_piqi_spouse conf base p fam compute_sosa)
      sl
  in
  let ascend = get_parents p <> None in
  let descend =
    List.exists
      (fun c -> Array.length (get_children c) > 0)
      (List.map (foi base) faml)
  in
  let baseprefix = conf.command in
  let visible = is_visible conf base p in
  let index = Int32.of_string @@ Gwdb.string_of_iper gen_p.key_index in
  {
    M.Person.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    image = image;
    birth_date = birth;
    birth_place = birth_place;
    baptism_date = baptism;
    baptism_place = baptism_place;
    death_date = death;
    death_place = death_place;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    spouses = sl;
    ascend = ascend;
    descend = descend;
    visible_for_visitors = visible;
    baseprefix = baseprefix;
  }


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_full :
             config -> base -> person -> bool ->
               (person -> Sosa.t) -> FullPerson              *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person (piqi)
                    (piqi) dont tous les champs sont complétés.
                    Les tests de droits d'accès sont fait dans cette fonction.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
      - base_loop : booléen pour savoir s'il y a une boucle dans la base.
      - compute_sosa : appel de soit SosaCache.get_single_sosa,
                                soit SosaCache.get_sosa_person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_full conf base p compute_sosa =
  let gen_p = Util.string_gen_person base (gen_person_of_person p) in
  let p_auth = authorized_age conf base p in
  let sosa_p = Sosa.to_string (compute_sosa p) in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname =
    if not p_auth && (is_hide_names conf p) then ""
    else gen_p.surname
  in
  let first_name =
    if not p_auth && (is_hide_names conf p) then ""
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (get_occ p) in
  let index = Int32.of_string @@ Gwdb.string_of_iper gen_p.key_index in
  let publicname = if gen_p.public_name = "" then None else Some gen_p.public_name in
  let aliases = gen_p.aliases in
  let qualifiers =
    if not p_auth && (is_hide_names conf p) then []
    else gen_p.qualifiers
  in
  let firstname_aliases = gen_p.first_names_aliases in
  let surname_aliases = gen_p.surnames_aliases in
  let image =
    match Image.get_portrait conf base p with
    | Some src ->
        if gen_p.image <> "" then gen_p.image else Image.src_to_string src
    | None -> ""
  in
  let birth =
    match Date.od_of_cdate gen_p.birth with
    | Some d when p_auth -> Some (string_of_date d)
    | _ -> None
  in
  let birth_place =
    if p_auth then Some gen_p.birth_place
    else None
  in
  let birth_src =
    if p_auth then Some gen_p.birth_src
    else None
  in
  let baptism =
    match Date.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> Some (string_of_date d)
    | _ -> None
  in
  let baptism_place =
    if p_auth then Some gen_p.baptism_place
    else None
  in
  let baptism_src =
    if p_auth then Some gen_p.baptism_src
    else None
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, None)
      | Death (_, cd) ->
          let d = Date.date_of_cdate cd in
          (`dead, Some (string_of_date d))
      | DeadYoung -> (`dead_young, None)
      | DeadDontKnowWhen -> (`dead_dont_know_when, None)
      | DontKnowIfDead -> (`dont_know_if_dead, None)
      | OfCourseDead -> (`of_course_dead, None)
    else
      (`not_dead, None)
  in
  let death_place =
    if p_auth then Some gen_p.death_place
    else None
  in
  let death_src =
    if p_auth then Some gen_p.death_src
    else None
  in
  let burial =
    match get_burial p with
    | Buried cod | Cremated cod ->
        (match Date.od_of_cdate cod with
        | Some d when p_auth -> Some (string_of_date d)
        | _ -> None)
    | _ -> None
  in
  let burial_place =
    if p_auth then Some gen_p.burial_place
    else None
  in
  let burial_src =
    if p_auth then Some gen_p.burial_src
    else None
  in
  let titles = List.map title_to_piqi_title gen_p.titles in
  let occupation =
    if p_auth then Some gen_p.occupation
    else None
  in
  let psources =
    if p_auth then Some gen_p.psources
    else None
  in
  let related = List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (get_related p) in
  let rparents =
    List.map
      (fun rp ->
        let father =
          match rp.r_fath with
          | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
          | None -> None
        in
        let mother =
          match rp.r_moth with
          | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
          | None -> None
        in
        let source = rp.r_sources in
        let rpt_type =
          match rp.r_type with
          | Adoption -> `rpt_adoption
          | Recognition -> `rpt_recognition
          | CandidateParent -> `rpt_candidate_parent
          | GodParent -> `rpt_god_parent
          | FosterParent -> `rpt_foster_parent
        in
        M.Relation_parent.({
          father = father;
          mother = mother;
          source = if source = "" then None else Some source;
          rpt_type = rpt_type;
        }))
      gen_p.rparents
  in
  let families =
    Mutil.array_to_list_map (fun x -> Int32.of_string @@ Gwdb.string_of_ifam x) (get_family p)
  in
  let parents =
    match get_parents p with
     | Some ifam -> Some (Int32.of_string (Gwdb.string_of_ifam ifam))
     | None -> None
  in
  let baseprefix = conf.command
  in
  let visible = is_visible conf base p in
  {
    M.Full_person.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    aliases = aliases;
    qualifiers = qualifiers;
    firstname_aliases = firstname_aliases;
    surname_aliases = surname_aliases;
    image = if image = "" then None else Some image;
    birth_date = birth;
    birth_place = birth_place;
    birth_src = birth_src;
    baptism_date = baptism;
    baptism_place = baptism_place;
    baptism_src = baptism_src;
    death_date = death;
    death_place = death_place;
    death_src = death_src;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    burial_src = burial_src;
    occupation = occupation;
    psources = psources;
    titles = titles;
    related = related;
    rparents = rparents;
    visible_for_visitors = visible;
    parents = parents;
    families = families;
    baseprefix = baseprefix;
  }


let pers_to_piqi_person conf base p compute_sosa =
  if p_getenvbin conf.env "full_infos" = Some "1"
  then PFull (pers_to_piqi_person_full conf base p compute_sosa)
  else PLight (pers_to_piqi_person_light conf base p compute_sosa)


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family : config -> base -> ifam -> Full_family    *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family conf base ifam =
  let fam = foi base ifam in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  let index = Int32.of_string @@ Gwdb.string_of_ifam ifam in
  let fsources =
    if m_auth then Some gen_f.fsources
    else None
  in
  let marriage =
    match Date.od_of_cdate gen_f.marriage with
    | Some d when m_auth -> Some (string_of_date d)
    | _ -> None
  in
  let marriage_place =
    if m_auth then Some gen_f.marriage_place
    else None
  in
  let marriage_src =
    if m_auth then Some gen_f.marriage_src
    else None
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Date.od_of_cdate cod with
         | Some d when m_auth -> (`divorced, Some (string_of_date d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  let witnesses =
    List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Array.to_list gen_f.witnesses)
  in
  let father = Int32.of_string @@ Gwdb.string_of_iper ifath in
  let mother = Int32.of_string @@ Gwdb.string_of_iper imoth in
  let children =
    List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Array.to_list (get_children fam))
  in
  {
    M.Full_family.fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    witnesses = witnesses;
    father = father;
    mother = mother;
    children = children;
    index = index;
  }


(* *********************************************************************** *)
(*  [Fonc] fam_to_piqi_family_link : config -> base -> ifam -> Full_family *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnÃ©e
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exportÃ© en clair hors de ce module.                        *)
(* *********************************************************************** *)
let fam_to_piqi_family_link base (ifath, imoth) ifam fam =
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = (Int32.of_string @@ Gwdb.string_of_ifam ifam) in
  let fsources = None in
  let marriage =
    match Date.od_of_cdate gen_f.marriage with
    | Some d -> Some (string_of_date d)
    | _ -> None
  in
  let marriage_place = Some gen_f.marriage_place in
  let marriage_src = Some gen_f.marriage_src in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Date.od_of_cdate cod with
         | Some d -> (`divorced, Some (string_of_date d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  let witnesses = [] in
  let father = Int32.of_string @@ Gwdb.string_of_iper ifath in
  let mother = Int32.of_string @@ Gwdb.string_of_iper imoth in
  (* TODO ? *)
  let children = [] in
  {
    M.Full_family.fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    witnesses = witnesses;
    father = father;
    mother = mother;
    children = children;
    index = index;
  }


(**/**) (* Fonctions de conversion *)

let data_person p =
  match p with
  | PLight p -> Mext.gen_person p
  | PFull p -> Mext.gen_full_person p

let person_map conf base l compute_sosa =
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull
      (List.map
         (fun p -> pers_to_piqi_person_full conf base p compute_sosa)
         l)
  else
    PLight
      (List.map
         (fun p -> pers_to_piqi_person_light conf base p compute_sosa)
         l)

let conv_data_list_person conf base filters l =
  let len = List.length l in
  if filters.nb_results then
    let len = M.Internal_int32.({value = Int32.of_int len}) in
    Mext.gen_internal_int32 len
  else
    let compute_sosa = compute_sosa conf base (len <= 1) in
    let l = person_map conf base l compute_sosa in
    match l with
    | PLight pl ->
        let list = M.List_persons.({list_persons = pl}) in
        Mext.gen_list_persons list
    | PFull pl ->
        let list = M.List_full_persons.({persons = pl}) in
        Mext.gen_list_full_persons list

let data_list_person_option conf base filters l =
  let len = List.length l in
  let compute_sosa = compute_sosa conf base (len <= 1) in
  if filters.nb_results then
    let len = M.Internal_int32.({value = Int32.of_int len}) in
    Mext.gen_internal_int32 len
  else
    let l =
      if p_getenvbin conf.env "full_infos" = Some "1" then
        PFull
          (List.map
            (fun p ->
              match p with
              | PFull p ->
                  if apply_filters_p conf filters compute_sosa p then
                    pers_to_piqi_person_full conf base p compute_sosa
                  else
                    let ref_p = person_to_reference_person base p in
                    empty_piqi_person_full conf ref_p
              | PLight ref_p -> empty_piqi_person_full conf ref_p)
            l)
      else
        PLight
          (List.map
            (fun p ->
              match p with
              | PFull p ->
                  if apply_filters_p conf filters compute_sosa p then
                    pers_to_piqi_person_light conf base p compute_sosa
                  else
                    let ref_p = person_to_reference_person base p in
                    empty_piqi_person_light conf ref_p
              | PLight ref_p -> empty_piqi_person_light conf ref_p)
            l)
    in
    match l with
    | PLight pl ->
        let list = M.List_persons.({list_persons = pl}) in
        Mext.gen_list_persons list
    | PFull pl ->
        let list = M.List_full_persons.({persons = pl}) in
        Mext.gen_list_full_persons list

let person_node_map conf base l =
  let len = List.length l in
  let compute_sosa = compute_sosa conf base (len <= 1) in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull
      (List.rev_map
         (fun p ->
           let id = Int64.of_string @@ Gwdb.string_of_iper (get_iper p) in
           let p =
             pers_to_piqi_person_full conf base p compute_sosa
           in
           M.Full_node.({
             id = id;
             person = p;
           }))
         l)
  else
    PLight
      (List.rev_map
         (fun p ->
           let id = Int64.of_string @@ Gwdb.string_of_iper (get_iper p) in
           let p =
             pers_to_piqi_person_light conf base p compute_sosa
           in
           M.Node.({
             id = id;
             person = p;
           }))
         l)
  
let person_node_map_lia conf base l =
  let compute_sosa = (fun _ -> Sosa.zero) in
  (* TODO ? *)
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull
      (List.rev_map
         (fun (id, p) ->
           let p =
             pers_to_piqi_person_full conf base p compute_sosa
           in
           M.Full_node.({
             id = id;
             person = p;
           }))
         l)
  else
    PLight
      (List.rev_map
         (fun (id, p) ->
           let p =
             pers_to_piqi_person_light conf base p compute_sosa
           in
           M.Node.({
             id = id;
             person = p;
           }))
         l)

let chop_base_prefix base_prefix =
  let len = String.length base_prefix in
  if len > 2 &&
     (base_prefix.[len-1] = 'w' || base_prefix.[len-1] = 'f') &&
     base_prefix.[len-2] = '_'
  then
    String.sub base_prefix 0 (len - 2)
  else base_prefix


let print_error = Api_piqi_util.print_error

let witness_kind_of_piqi = function
  | `witness                  -> Witness
  | `witness_godparent        -> Witness_GodParent
  | `witness_civilofficer     -> Witness_CivilOfficer
  | `witness_religiousofficer -> Witness_ReligiousOfficer
  | `witness_informant        -> Witness_Informant
  | `witness_attending        -> Witness_Attending
  | `witness_mentioned        -> Witness_Mentioned
  | `witness_other            -> Witness_Other

let piqi_of_witness_kind = function
  | Witness                  -> `witness
  | Witness_GodParent        -> `witness_godparent
  | Witness_CivilOfficer     -> `witness_civilofficer
  | Witness_ReligiousOfficer -> `witness_religiousofficer
  | Witness_Informant        -> `witness_informant
  | Witness_Attending        -> `witness_attending
  | Witness_Mentioned        -> `witness_mentioned
  | Witness_Other            -> `witness_other

let translate_witness conf witness_kind =
  Util.string_of_witness_kind conf Def.Neuter witness_kind

let witness_kinds = [
    Witness;
    Witness_GodParent;
    Witness_CivilOfficer;
    Witness_ReligiousOfficer;
    Witness_Informant;
    Witness_Attending;
    Witness_Mentioned;
    Witness_Other
  ]

let piqi_death_type_of_death = function
  | NotDead -> `not_dead
  | DontKnowIfDead -> `dont_know_if_dead
  | OfCourseDead -> `of_course_dead
  | Death _ | DeadDontKnowWhen -> `dead
  | DeadYoung -> `dead_young

let opt_of_string = function
  | "" -> None
  | s -> Some s
