let reconstitute_person_aux conf fn_occ fn_rparents fn_pevt_witnesses mod_p =
  let key_index = Gwdb.iper_of_string @@ Int32.to_string mod_p.Api_update_util.Mwrite.Person.index in
  let first_name = Geneweb.Util.only_printable mod_p.Api_update_util.Mwrite.Person.firstname in
  let surname = Geneweb.Util.only_printable mod_p.Api_update_util.Mwrite.Person.lastname in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if (List.exists contain_fn Name.forbidden_char)
    || (List.exists contain_sn Name.forbidden_char)
    then (Name.purge first_name, Name.purge surname)
    else (first_name, surname)
  in
  let occ = fn_occ mod_p in
  let image = Option.fold ~none:"" ~some:Geneweb.Util.only_printable mod_p.Api_update_util.Mwrite.Person.image in
  let strings_aux = List.map Geneweb.Util.only_printable in
  let first_names_aliases = strings_aux mod_p.Api_update_util.Mwrite.Person.firstname_aliases in
  let surnames_aliases = strings_aux mod_p.Api_update_util.Mwrite.Person.surname_aliases in
  let public_name = Option.value ~default:"" mod_p.Api_update_util.Mwrite.Person.public_name |> Geneweb.Util.only_printable in
  let qualifiers = strings_aux mod_p.Api_update_util.Mwrite.Person.qualifiers in
  let aliases = strings_aux mod_p.Api_update_util.Mwrite.Person.aliases in
  let titles =
    List.map begin fun t ->
      { Def.t_name =
          begin match t.Api_update_util.Mwrite.Title.name with
            | Some s -> if s = "" then Tnone else Tname s
            | None -> Tnone
          end
      ; t_ident = begin match t.Api_update_util.Mwrite.Title.title with
          | Some s -> s
          | None -> ""
        end
      ; t_place = begin match t.Api_update_util.Mwrite.Title.fief with
          | Some s -> s
          | None -> ""
        end
      ; t_date_start = begin match t.Api_update_util.Mwrite.Title.date_begin with
          | Some date -> Api_update_util.date_of_piqi_date conf date |> Date.cdate_of_od
          | None -> Date.cdate_None
        end
      ; t_date_end = begin match t.Api_update_util.Mwrite.Title.date_end with
          | Some date -> Api_update_util.date_of_piqi_date conf date |> Date.cdate_of_od
          | None -> Date.cdate_None
        end
      ; t_nth = begin match t.Api_update_util.Mwrite.Title.nth with
          | Some i -> Int32.to_int i
          | None -> 0
        end
      }
    end mod_p.Api_update_util.Mwrite.Person.titles
  in
  let rparents = fn_rparents mod_p in
  let access = Api_piqi_util.piqi_access_to_access mod_p.Api_update_util.Mwrite.Person.access in
  let occupation = Option.fold ~none:"" ~some:Geneweb.Util.only_printable mod_p.Api_update_util.Mwrite.Person.occupation in
  let sex =
    match mod_p.Api_update_util.Mwrite.Person.sex with
    | `male -> Def.Male
    | `female -> Def.Female
    | `unknown -> Def.Neuter
  in
  let death =
    match mod_p.Api_update_util.Mwrite.Person.death_type with
    | `not_dead -> Def.NotDead
    | `dead -> Def.DeadDontKnowWhen
    | `dead_young -> Def.DeadYoung
    | `dead_dont_know_when -> Def.DeadDontKnowWhen
    | `dont_know_if_dead -> Def.DontKnowIfDead
    | `of_course_dead -> Def.OfCourseDead
  in
  let psources = Option.fold ~none:"" ~some:Geneweb.Util.only_printable mod_p.Api_update_util.Mwrite.Person.psources in
  let notes =
    Option.fold ~none:""
      ~some:(fun s -> Geneweb.Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces s))
      mod_p.Api_update_util.Mwrite.Person.notes
  in
  let original_pevents =
    (* GeneWeb used to strip empty death event, but we need to do it after conflicts check. *)
    List.map begin fun evt ->
      let name =
        match evt.Api_update_util.Mwrite.Pevent.event_perso with
        | Some n -> Def.Epers_Name (Geneweb.Util.only_printable n)
        | _ ->
          match evt.Api_update_util.Mwrite.Pevent.pevent_type with
          | Some x -> Api_piqi_util.pevent_name_of_piqi_pevent_name x
          | _ -> Def.Epers_Name ""
      in
      let date =
        match evt.Api_update_util.Mwrite.Pevent.date with
        | Some date -> Api_update_util.date_of_piqi_date conf date
        | None -> None
      in
      let place = Option.fold ~none:"" ~some:(fun p -> Geneweb.Util.only_printable p) evt.Api_update_util.Mwrite.Pevent.place in
      let reason = Option.fold ~none:"" ~some:(fun r -> Geneweb.Util.only_printable r) evt.Api_update_util.Mwrite.Pevent.reason in
      let note =
        Option.fold
          ~none:"" ~some:(fun n -> Geneweb.Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces n))
          evt.Api_update_util.Mwrite.Pevent.note
      in
      let src = Option.fold ~none:"" ~some:Geneweb.Util.only_printable evt.Api_update_util.Mwrite.Pevent.src in
      let witnesses = fn_pevt_witnesses evt in
      { Def.epers_name = name; epers_date = Date.cdate_of_od date;
        epers_place = place; epers_reason = reason; epers_note = note;
        epers_src = src; epers_witnesses = Array.of_list witnesses }
    end mod_p.Api_update_util.Mwrite.Person.pevents
  in
  let (bi, bp, de, bu, pevents) =
    (* [reconstitute_from_pevents] sorts pevents.
       We need to keep the original pevents list in case of error.  *)
    Geneweb.UpdateIndOk.reconstitute_from_pevents original_pevents false
      (Date.cdate_None, "", "", "")
      (Date.cdate_None, "", "", "")
      (death, "", "", "")
      (UnknownBurial, "", "", "")
  in
  let (birth, birth_place, birth_note, birth_src) = bi in
  let (baptism, baptism_place, baptism_note, baptism_src) = bp in
  let (death, death_place, death_note, death_src) = de in
  let (burial, burial_place, burial_note, burial_src) = bu in
  (* Maintenant qu'on a propagé les évènements, on a *)
  (* peut-être besoin de refaire un infer_death.     *)
  (* FIXME: do no use the _bb version *)
  let death =
    match death with
    | DontKnowIfDead ->
      Geneweb.Update.infer_death_bb conf (Date.od_of_cdate birth) (Date.od_of_cdate baptism)
    | _ -> death
  in
  ( original_pevents
  , { Def.first_name ; surname ; occ ; sex ; access
    ; image
    ; first_names_aliases ; surnames_aliases
    ; public_name ; qualifiers ; aliases
    ; titles
    ; rparents
    ; occupation
    ; related = []
    ; birth ; birth_place ; birth_note ; birth_src
    ; baptism ; baptism_place ; baptism_note ; baptism_src
    ; death ; death_place ; death_note ; death_src
    ; burial ; burial_place ; burial_note ; burial_src
    ; notes
    ; pevents
    ; psources
    ; key_index
    }
  )

let reconstitute_person conf base mod_p
  : ('a, string * string * int * Geneweb.Update.create * string, string) Def.gen_person =
  let fn_occ mod_p =
    match mod_p.Api_update_util.Mwrite.Person.create_link with
    | `create ->
      let fn = mod_p.Api_update_util.Mwrite.Person.firstname in
      let sn = mod_p.Api_update_util.Mwrite.Person.lastname in
      Api_update_util.api_find_free_occ base fn sn
    | _ ->
      (* Cas par défaut, i.e. modifier personne sans changer le occ. *)
      Option.fold ~none:0 ~some:Int32.to_int mod_p.Api_update_util.Mwrite.Person.occ
  in
  let fn_rparents mod_p =
    List.fold_right begin fun r accu ->
      match r.Api_update_util.Mwrite.Relation_parent.person with
      | None -> accu
      | Some person when person.lastname = "?" && person.firstname = "?" -> accu
      | Some person ->
        let r_type =
          match r.Api_update_util.Mwrite.Relation_parent.rpt_type with
          | `rpt_adoption_father | `rpt_adoption_mother -> Def.Adoption
          | `rpt_recognition_father | `rpt_recognition_mother -> Def.Recognition
          | `rpt_candidate_parent_father | `rpt_candidate_parent_mother -> Def.CandidateParent
          | `rpt_god_parent_father | `rpt_god_parent_mother -> Def.GodParent
          | `rpt_foster_parent_father | `rpt_foster_parent_mother -> Def.FosterParent
        in
        let (r_fath, r_moth) =
          match person.Api_update_util.Mwrite.Person_link.sex with
          | `female -> (None, Some (Api_update_util.reconstitute_somebody base person))
          | _ -> (Some (Api_update_util.reconstitute_somebody base person), None)
        in
        let r_sources =
          match r.Api_update_util.Mwrite.Relation_parent.source with
          | Some s -> s
          | None -> ""
        in
        let r =
          { Def.r_type = r_type; r_fath = r_fath;
            r_moth = r_moth; r_sources = r_sources }
        in
        r :: accu
    end mod_p.Api_update_util.Mwrite.Person.rparents []
  in
  let fn_pevt_witnesses evt =
    List.fold_right begin fun witness accu ->
      match witness.Api_update_util.Mwrite.Witness.person with
      | Some person ->
        let wk = Api_util.witness_kind_of_piqi witness.Api_update_util.Mwrite.Witness.witness_type in
        let wnote = witness.Api_update_util.Mwrite.Witness.witness_note in
        let wnote = Option.fold ~none:"" ~some:(fun x -> x) wnote in
        let wit = (Api_update_util.reconstitute_somebody base person, wk, wnote) in
        wit :: accu
      | None -> accu
    end evt.Api_update_util.Mwrite.Pevent.witnesses []
  in
  let original_pevents, p = reconstitute_person_aux conf fn_occ fn_rparents fn_pevt_witnesses mod_p in
  ignore @@ Api_update_util.check_person_conflict base original_pevents p ;
  (* Now, trim and format events *)
  let pevents =
    Mutil.filter_map begin function
      | { Def.epers_name = Def.Epers_Death
        ; epers_place = ""
        ; epers_reason = ""
        ; epers_note = ""
        ; epers_src = ""
        ; epers_witnesses = [||]
        ; epers_date
        }
        when epers_date = Date.cdate_None && p.death = DontKnowIfDead -> None
      | e ->
        Some { e
               with epers_witnesses =
                      Array.map begin fun ((f, s, o, create, var, _), wk, wnote) ->
                        ((f, s, o, create, var), wk, wnote)
                      end e.Def.epers_witnesses
             }
    end p.pevents
  in
  let rparents =
    List.map begin fun r ->
      let (r_fath, r_moth) =
        match (r.Def.r_fath, r.Def.r_moth) with
        | (Some (f, s, o, create, var, _), None) ->
          (Some (f, s, o, create, var), None)
        | (None, Some (f, s, o, create, var, _)) ->
          (None, Some (f, s, o, create, var))
        | _ -> failwith "rparents_gw"
      in
      { r  with Def.r_fath ; r_moth }
    end p.rparents
  in
  { p with Def.rparents ; pevents ; related = [] }

(**/**)


let print_add conf base mod_p =
  try
    let sp : ('a, string * string * int * Geneweb.Update.create * string, string) Def.gen_person = reconstitute_person conf base mod_p in
    let sp = {(sp) with key_index = Gwdb.dummy_iper} in
    (* On met à jour les occ. *)
    if sp.occ <> 0 then mod_p.Api_update_util.Mwrite.Person.occ <- Some (Int32.of_int sp.occ);
    let sp : ('a, string * string * int * Geneweb.Update.create * string, string) Def.gen_person = Geneweb.UpdateIndOk.strip_person sp in
    match Geneweb.UpdateIndOk.check_person conf base (sp : ('a, string * string * int * Geneweb.Update.create * string, string) Def.gen_person) with
    | Some err ->
        (* Correspond au cas ou fn/sn = ""/"?" *)
        (* => ne devrait pas se produire       *)
        Api_update_util.UpdateError err
    | None ->
        let (p, a) = Geneweb.UpdateIndOk.effective_add conf base (sp : ('a, string * string * int * Geneweb.Update.create * string, string) Def.gen_person) in
        let u = {Def.family = Gwdb.get_family (Gwdb.poi base p.key_index)} in
        let wl = Geneweb.UpdateIndOk.all_checks_person base p a u in
        let changed = Def.U_Add_person (Geneweb.Util.string_gen_person base p) in
        let hr = [(fun () -> Geneweb.History.record conf base changed "ap")] in
        let created_person = Api_update_util.created_person_of_person base p in
        Api_update_util.UpdateSuccess (wl, [], hr, Some created_person)
  with
  | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c

let print_mod_aux conf base ncn mod_p callback =
  try
    let p : ('a, string * string * int * Geneweb.Update.create * string, string) Def.gen_person =
      reconstitute_person conf base mod_p
    in
    let p = Geneweb.UpdateIndOk.strip_person p in
    let ini_ps = Geneweb.UpdateInd.string_person_of base (Gwdb.poi base p.key_index) in
    let digest = Geneweb.Update.digest_person ini_ps in
    if digest = mod_p.Api_update_util.Mwrite.Person.digest then
      match match if ncn then None else Geneweb.Update.check_missing_name base p with
        | Some _ as err -> err
        | None ->
          let missing_wit_names_o =
            Geneweb.Update.check_missing_witnesses_names conf (fun e -> e.Def.epers_witnesses) p.pevents
          in
          if Option.is_none missing_wit_names_o then
            Geneweb.Update.check_illegal_access_update base p
          else missing_wit_names_o
      with
      | Some err -> Api_update_util.UpdateError err
      | None -> callback p
    else
      Geneweb.Update.error_digest conf
  with
  | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c

let print_mod ?(no_check_name = false) ?(fexclude = []) conf base mod_p =
  let ip = Gwdb.iper_of_string @@ Int32.to_string mod_p.Api_update_util.Mwrite.Person.index in
  let o_p =
    Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person (Gwdb.poi base ip))
  in
  let callback p =
    begin
      let p =
        (* Do not check sex of married person *)
        let conf = { conf with Geneweb.Config.env = ("nsck", Adef.encoded "on") :: conf.Geneweb.Config.env } in
        Geneweb.UpdateIndOk.effective_mod conf base p
      in
      let op = Gwdb.poi base p.key_index in
      let u = {Def.family = Gwdb.get_family op} in
      Gwdb.patch_person base p.key_index p;
      let s =
        let sl =
          [p.notes; p.occupation; p.birth_note; p.birth_src; p.baptism_note;
           p.baptism_src; p.death_note; p.death_src; p.burial_note;
           p.burial_src; p.psources]
        in
        let sl =
          let rec loop l accu =
            match l with
            | [] -> accu
            | evt :: l -> loop l (evt.Def.epers_note :: evt.Def.epers_src :: accu)
          in
          loop (p.pevents) sl
        in
        String.concat " " (List.map (Gwdb.sou base) sl)
      in
      Geneweb.Notes.update_notes_links_db base (Def.NLDB.PgInd p.key_index) s;
      let wl =
        let a = Gwdb.poi base p.key_index in
        let a = {Def.parents = Gwdb.get_parents a; consang = Gwdb.get_consang a} in
        let family =
          Array.of_list @@
          Array.fold_right begin fun ifam acc ->
            if List.mem ifam fexclude then acc else ifam :: acc
          end u.family []
        in
        let u = { Def.family } in
        Geneweb.UpdateIndOk.all_checks_person base p a u
      in
      let changed = Def.U_Modify_person (o_p, (Geneweb.Util.string_gen_person base p)) in
      let hr =
        [(fun () -> Geneweb.History.record conf base changed "mp");
         (fun () ->
           if not (Gwdb.is_quest_string p.surname) &&
              not (Gwdb.is_quest_string p.first_name) &&
              not (Geneweb.Util.is_old_person conf p)
           then
             Geneweb.Update.delete_topological_sort_v conf base
           else ())]
      in
      let created_person = Api_update_util.created_person_of_person base p in
      Api_update_util.UpdateSuccess (wl, [], hr, Some created_person)
    end
  in
  print_mod_aux conf base no_check_name mod_p callback


(**/**) (* Fonctions pour la première saisie, i.e. on n'a pas de base ! *)


(* Comme on n'a pas de base, on va garder une hashtbl des occurrences. *)
let ht_occ = Hashtbl.create 7 ;;

let find_free_occ_nobase fn sn =
  let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  try
    let occ = Hashtbl.find ht_occ key in
    Hashtbl.replace ht_occ key (succ occ);
    occ
  with Not_found ->
    begin
      let occ = 0 in
      Hashtbl.add ht_occ key (succ occ);
      occ
    end

let reconstitute_person_nobase conf mod_p =
  let fn_occ mod_p =
    match mod_p.Api_update_util.Mwrite.Person.create_link with
    | `create_default_occ ->
      let fn = mod_p.Api_update_util.Mwrite.Person.firstname in
      let sn = mod_p.Api_update_util.Mwrite.Person.lastname in
      find_free_occ_nobase fn sn
    | `create ->
      begin match mod_p.Api_update_util.Mwrite.Person.occ with
        | Some occ -> Int32.to_int occ
        | None -> 0
      end
    | `link ->
      failwith "ErrorAddPersonNoBase"
  in
  let fn_rparents _ = [] in
  let fn_pevt_witnesses _ = [] in
  let _, p = reconstitute_person_aux conf fn_occ fn_rparents fn_pevt_witnesses mod_p in
  { p with Def.pevents = List.filter begin fun e ->
        e.Def.epers_name <> Def.Epers_Death
        || e.Def.epers_place <> ""
        || e.Def.epers_reason <> ""
        || e.Def.epers_note <> ""
        || e.Def.epers_src <> ""
        || e.Def.epers_witnesses <> [||]
        || e.Def.epers_date <> Date.cdate_None
        || p.Def.death = Def.DontKnowIfDead
      end p.pevents }

let print_add_nobase conf mod_p =
  try
    let sp = reconstitute_person_nobase conf mod_p in
    let sp = {(sp) with key_index = Gwdb.dummy_iper} in
    (* On met à jour les occ. *)
    if sp.occ <> 0 then mod_p.Api_update_util.Mwrite.Person.occ <- Some (Int32.of_int sp.occ);
    let _sp = Geneweb.UpdateIndOk.strip_person sp in
    (* On ne vérifie pas ici si le prénom de la personne est vide, mais *)
    (* on le fait plus haut, pour savoir si c'est un oubli ou si l'on   *)
    (* ne connait pas la personne.                                      *)
    (* On n'appelle pas CheckItem car ils ne sont pas révélateurs *)
    Api_update_util.UpdateSuccess ([], [], [], None)
  with
  | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
