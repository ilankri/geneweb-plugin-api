let opt_only_printable = function
  | Some s -> Geneweb.Util.only_printable s
  | None -> ""

let opt_only_printable_or_nl_stripped = function
  | Some x -> Geneweb.Util.only_printable_or_nl (Mutil.strip_all_trailing_spaces x)
  | None -> ""

let to_event_with_update_key (event : _ Def.gen_fam_event) =
  let to_witness_with_update_key (person, kind, note) =
    (Api_update_util.to_update_key person, kind, note)
  in
  {event with
    efam_witnesses = Array.map to_witness_with_update_key event.efam_witnesses}

let to_family_with_update_key (family : _ Def.gen_family) :
      (Geneweb.Update.key, Gwdb.ifam, string) Def.gen_family =
  {family with
    witnesses = Array.map Api_update_util.to_update_key family.witnesses;
    fevents = List.map to_event_with_update_key family.fevents}

let to_couple_with_update_key (couple : _ Def.gen_couple) :
      Geneweb.Update.key Def.gen_couple =
  Adef.couple
    (couple |> Adef.father |> Api_update_util.to_update_key)
    (couple |> Adef.mother |> Api_update_util.to_update_key)

let to_descend_with_update_key (descent: _ Def.gen_descend) :
      (Geneweb.Update.key) Def.gen_descend =
  {children = Array.map Api_update_util.to_update_key descent.children}

let reconstitute_family conf base mod_f =
  (* Attention, si witnesses est vide, on va supprimer des témoins (qui sont
     en double parce que dans GeneWeb, ils sont récupérés une fois dans fevents
     et une fois dans le bloc hidden. Du coup à la validation, on supprime
     les 'deuxième' témoins (voir update.ml) *)
  let witnesses =
    List.map
      (fun ip ->
        let p = Gwdb.poi base (Gwdb.iper_of_string @@ Int32.to_string ip) in
        let fn = Gwdb.sou base (Gwdb.get_first_name p) in
        let sn = Gwdb.sou base (Gwdb.get_surname p) in
        let occ =
          if fn = "?" || sn = "?"
          then int_of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p)
          else Gwdb.get_occ p
        in
        (fn, sn, occ, Geneweb.Update.Link, false))
      mod_f.Api_saisie_write_piqi.Family.old_witnesses
  in
  let fevents =
    List.map
      (fun evt ->
        let name =
          match evt.Api_saisie_write_piqi.Fevent.event_perso with
          | Some n -> Def.Efam_Name (Geneweb.Util.only_printable n)
          | _ ->
              match evt.Api_saisie_write_piqi.Fevent.fevent_type with
              | Some `efam_marriage -> Def.Efam_Marriage
              | Some `efam_no_marriage -> Def.Efam_NoMarriage
              | Some `efam_no_mention -> Def.Efam_NoMention
              | Some `efam_engage -> Def.Efam_Engage
              | Some `efam_divorce -> Def.Efam_Divorce
              | Some `efam_separated -> Def.Efam_Separated
              | Some `efam_annulation -> Def.Efam_Annulation
              | Some `efam_marriage_bann -> Def.Efam_MarriageBann
              | Some `efam_marriage_contract -> Def.Efam_MarriageContract
              | Some `efam_marriage_license -> Def.Efam_MarriageLicense
              | Some `efam_pacs -> Def.Efam_PACS
              | Some `efam_residence -> Def.Efam_Residence
              | _ -> Def.Efam_Name ""
        in
        let date =
          match evt.Api_saisie_write_piqi.Fevent.date with
          | Some d -> Api_update_util.date_of_piqi_date conf d
          | None -> None
        in
        let place = opt_only_printable evt.Api_saisie_write_piqi.Fevent.place in
        let reason = opt_only_printable evt.Api_saisie_write_piqi.Fevent.reason in
        let note = opt_only_printable_or_nl_stripped evt.Api_saisie_write_piqi.Fevent.note in
        let src = opt_only_printable evt.Api_saisie_write_piqi.Fevent.src  in
        let witnesses =
          List.fold_right
            (fun witness accu ->
              match witness.Api_saisie_write_piqi.Witness.person with
              | Some person ->
                  let wk = Api_util.witness_kind_of_piqi witness.Api_saisie_write_piqi.Witness.witness_type in
                  let wnote = witness.Api_saisie_write_piqi.Witness.witness_note in
                  let wnote = Option.fold ~none:"" ~some:(fun x -> x) wnote in
                  let wit = (Api_update_util.reconstitute_somebody base person, wk, wnote) in
                  wit :: accu
              | None -> accu)
            evt.Api_saisie_write_piqi.Fevent.witnesses []
        in
        { Def.efam_name = name; efam_date = Date.cdate_of_od date;
          efam_place = place; efam_reason = reason; efam_note = note;
          efam_src = src; efam_witnesses = Array.of_list witnesses })
      mod_f.Api_saisie_write_piqi.Family.fevents
  in
  let comment =
    opt_only_printable_or_nl_stripped mod_f.Api_saisie_write_piqi.Family.comment
  in
  let fsources = opt_only_printable mod_f.Api_saisie_write_piqi.Family.fsources in
  let origin_file = Option.value ~default:"" mod_f.Api_saisie_write_piqi.Family.origin_file in
  let fam_index = Gwdb.ifam_of_string @@ Int32.to_string mod_f.Api_saisie_write_piqi.Family.index in
  let parents =
    let father = mod_f.Api_saisie_write_piqi.Family.father in
    let sex =
      match father.Api_saisie_write_piqi.Person.sex with
      | `male -> Def.Male
      | `female -> Def.Female
      | `unknown -> Def.Neuter
    in
    let father =
      match father.Api_saisie_write_piqi.Person.create_link with
      | `create_default_occ ->
          let fn = father.Api_saisie_write_piqi.Person.firstname in
          let sn = father.Api_saisie_write_piqi.Person.lastname in
          let occ =
            match father.Api_saisie_write_piqi.Person.occ with
            | Some occ -> Int32.to_int occ
            | None -> 0
          in
          (fn, sn, occ, Geneweb.Update.Create (sex, None), false)
      | `create ->
          let fn = father.Api_saisie_write_piqi.Person.firstname in
          let sn = father.Api_saisie_write_piqi.Person.lastname in
          let occ = Api_update_util.api_find_free_occ base fn sn in
          (* On met à jour parce que si on veut le rechercher, *)
          (* il faut qu'on connaisse son occ.                  *)
          let () =
            if occ = 0 then father.Api_saisie_write_piqi.Person.occ <- None
            else father.Api_saisie_write_piqi.Person.occ <- Some (Int32.of_int occ)
          in
          (fn, sn, occ, Geneweb.Update.Create (sex, None), true)
      | `link ->
          let ip = Gwdb.iper_of_string @@ Int32.to_string father.Api_saisie_write_piqi.Person.index in
          let p = Gwdb.poi base ip in
          let fn = Gwdb.sou base (Gwdb.get_first_name p) in
          let sn = Gwdb.sou base (Gwdb.get_surname p) in
          let occ =
            if fn = "?" || sn = "?"
            then int_of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p)
            else Gwdb.get_occ p
          in
          (fn, sn, occ, Geneweb.Update.Link, false)
    in
    let mother = mod_f.Api_saisie_write_piqi.Family.mother in
    let sex =
      match mother.Api_saisie_write_piqi.Person.sex with
      | `male -> Def.Male
      | `female -> Def.Female
      | `unknown -> Def.Neuter
    in
    let mother =
      match mother.Api_saisie_write_piqi.Person.create_link with
      | `create_default_occ ->
          let fn = mother.Api_saisie_write_piqi.Person.firstname in
          let sn = mother.Api_saisie_write_piqi.Person.lastname in
          let occ =
            match mother.Api_saisie_write_piqi.Person.occ with
            | Some occ -> Int32.to_int occ
            | None -> 0
          in
          (fn, sn, occ, Geneweb.Update.Create (sex, None), false)
      | `create ->
          let fn = mother.Api_saisie_write_piqi.Person.firstname in
          let sn = mother.Api_saisie_write_piqi.Person.lastname in
          let occ = Api_update_util.api_find_free_occ base fn sn in
          (* On met à jour parce que si on veut le rechercher, *)
          (* il faut qu'on connaisse son occ.                  *)
          let () =
            if occ = 0 then mother.Api_saisie_write_piqi.Person.occ <- None
            else mother.Api_saisie_write_piqi.Person.occ <- Some (Int32.of_int occ)
          in
          (fn, sn, occ, Geneweb.Update.Create (sex, None), true)
      | `link ->
          let ip = Gwdb.iper_of_string @@ Int32.to_string mother.Api_saisie_write_piqi.Person.index in
          let p = Gwdb.poi base ip in
          let fn = Gwdb.sou base (Gwdb.get_first_name p) in
          let sn = Gwdb.sou base (Gwdb.get_surname p) in
          let occ =
            if fn = "?" || sn = "?"
            then int_of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p)
            else Gwdb.get_occ p
          in
          (fn, sn, occ, Geneweb.Update.Link, false)
    in
    [father; mother]
  in
  let children =
    List.map
      (fun child ->
         match child.Api_saisie_write_piqi.Person_link.create_link with
         | `create_default_occ ->
             let sex =
               match child.Api_saisie_write_piqi.Person_link.sex with
               | `male -> Def.Male
               | `female -> Def.Female
               | `unknown -> Def.Neuter
             in
             let fn = child.Api_saisie_write_piqi.Person_link.firstname in
             let sn = child.Api_saisie_write_piqi.Person_link.lastname in
             let occ =
               match child.Api_saisie_write_piqi.Person_link.occ with
               | Some occ -> Int32.to_int occ
               | None -> 0
             in
             (fn, sn, occ, Geneweb.Update.Create (sex, None), false)
         | `create ->
             let sex =
               match child.Api_saisie_write_piqi.Person_link.sex with
               | `male -> Def.Male
               | `female -> Def.Female
               | `unknown -> Def.Neuter
             in
             let fn = child.Api_saisie_write_piqi.Person_link.firstname in
             let sn = child.Api_saisie_write_piqi.Person_link.lastname in
             let occ = Api_update_util.api_find_free_occ base fn sn in
             (* On met à jour parce que si on veut le rechercher, *)
             (* il faut qu'on connaisse son occ.                  *)
             let () =
               if occ = 0 then child.Api_saisie_write_piqi.Person_link.occ <- None
               else child.Api_saisie_write_piqi.Person_link.occ <- Some (Int32.of_int occ)
             in
             (fn, sn, occ, Geneweb.Update.Create (sex, None), true)
         | `link ->
             let ip = Gwdb.iper_of_string @@ Int32.to_string child.Api_saisie_write_piqi.Person_link.index in
             let p = Gwdb.poi base ip in
             let fn = Gwdb.sou base (Gwdb.get_first_name p) in
             let sn = Gwdb.sou base (Gwdb.get_surname p) in
             let occ =
               if fn = "?" || sn = "?"
               then int_of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p)
               else Gwdb.get_occ p
             in
             (fn, sn, occ, Geneweb.Update.Link, false))
      mod_f.Api_saisie_write_piqi.Family.children
  in
  (* Attention, surtout pas les witnesses, parce que si on en créé un, *)
  (* on le créé aussi dans witness et on ne pourra jamais valider.     *)
  let ((relation, marriage, marriage_place, marriage_note, marriage_src),
       divorce,
       _) =
    Geneweb.UpdateFamOk.reconstitute_from_fevents (Geneweb.Util.p_getenv conf.env "nsck" = Some "on") "" fevents
  in
  (* Si parents de même sex ... *)
  let relation =
    let father = mod_f.Api_saisie_write_piqi.Family.father in
    let mother = mod_f.Api_saisie_write_piqi.Family.mother in
    match (father.Api_saisie_write_piqi.Person.sex, mother.Api_saisie_write_piqi.Person.sex) with
    | (`male, `male) | (`female, `female) ->
        (match relation with
         | Married -> Def.NoSexesCheckMarried
         | _ -> Def.NoSexesCheckNotMarried)
    | _ -> relation
  in
  (* => pour l'instant, CheckItem ne vérifie pas le sex des parents. *)
  let fam =
    {Def.marriage; marriage_place;
     marriage_note; marriage_src;
     fevents; witnesses = Array.of_list witnesses;
     relation; divorce; comment;
     origin_file; fsources; fam_index}
  and cpl = Futil.parent conf.multi_parents (Array.of_list parents)
  and des = {Def.children = Array.of_list children} in
  (* On vérifie s'il y a des conflits de personne. *)
  (* Normalement, il ne doit plus y avoir de lever *)
  (* de conflits par les autres modules : update,  *)
  (* updateIndOk et updateFamOk.                   *)
  let _err = Api_update_util.check_family_conflict base fam cpl des in
  (* Maintenant qu'on a fini les conflit, on remet l'objet person *)
  (* tel que pour GeneWeb, c'est à dire qu'on supprime l'option   *)
  (* force_create.                                                *)
  let witnesses_gw =
    List.map
      (fun (f, s, o, create, _) -> (f, s, o, create))
      witnesses
  in
  let fevents_gw =
    List.map
      (fun e ->
        let w =
          Array.map
            (fun ((f, s, o, create, _), wk, _wnote) ->
              ((f, s, o, create), wk, _wnote))
            e.Def.efam_witnesses
        in
        {(e) with Def.efam_witnesses = w})
      fevents
  in
  let parents_gw =
    List.map
      (fun (f, s, o, create, _) -> (f, s, o, create))
      parents
  in
  let children_gw =
    List.map
      (fun (f, s, o, create, _) -> (f, s, o, create))
      children
  in
  let fam =
    {(fam) with witnesses = Array.of_list witnesses_gw; fevents = fevents_gw}
  in
  let cpl = Futil.parent conf.multi_parents (Array.of_list parents_gw) in
  let des = {Def.children = Array.of_list children_gw} in
  ((to_family_with_update_key fam :
      (Geneweb.Update.key, Gwdb.ifam, string) Def.gen_family),
   to_couple_with_update_key cpl,
   to_descend_with_update_key des)


(**/**)


let print_add conf base mod_f mod_fath mod_moth =
  (try
    let (sfam, scpl, sdes) = reconstitute_family conf base mod_f in
      (match Geneweb.UpdateFamOk.check_family conf sfam scpl sdes with
      | (Some err, _, _) | (_, Some err, _) | (_, _, Some err) ->
          (* Correspond au cas ou fn/sn = ""/"?" *)
          (* => ne devrait pas se produire       *)
          None, Api_update_util.UpdateError err
      | (None, None, None) ->
          begin
            let (sfam, sdes) = Geneweb.UpdateFamOk.strip_family sfam sdes in
            let (ifam, fam, cpl, des) =
              Geneweb.UpdateFamOk.effective_add conf base true sfam scpl sdes
            in
            let () = Geneweb.UpdateFamOk.patch_parent_with_pevents base cpl in
            let () = Geneweb.UpdateFamOk.patch_children_with_pevents base des in
            (* On met à jour les index ! et le digest ! *)
            let () =
              let fam = Gwdb.family_of_gen_family base (fam, cpl, des) in

              let ifath = Gwdb.get_father fam in
              let imoth = Gwdb.get_mother fam in

              let father = Gwdb.poi base ifath in
              let mother = Gwdb.poi base imoth in

              mod_f.Api_saisie_write_piqi.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam ifam;
              mod_fath.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ifath;

              let fath_occ = Gwdb.get_occ father in

              mod_fath.Api_saisie_write_piqi.Person.occ <-
                if fath_occ = 0 then None else Some (Int32.of_int fath_occ);
              mod_moth.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper imoth;

              let moth_occ = Gwdb.get_occ mother in

              mod_moth.Api_saisie_write_piqi.Person.occ <-
                if moth_occ = 0 then None else Some (Int32.of_int moth_occ);
              let digest_father =
                Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base father)
              in
              mod_fath.Api_saisie_write_piqi.Person.digest <- digest_father;
              let digest_mother =
                Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base mother)
              in
              mod_moth.Api_saisie_write_piqi.Person.digest <- digest_mother;
              mod_f.Api_saisie_write_piqi.Family.father <- mod_fath;
              mod_f.Api_saisie_write_piqi.Family.mother <- mod_moth;
            in
            (* TODO ?? idem enfant/witness ? *)
            (* optim ? regarder que ceux dont index = 0 *)
            let (wl, ml) =
              Geneweb.UpdateFamOk.all_checks_family
                conf base ifam fam cpl des (scpl, sdes, None)
            in
            (* TODO *)
            let (changed, act) =
              let fam = Geneweb.Util.string_gen_family base fam in
              let (ip, act) =
                match Geneweb.Util.p_getenv conf.env "ip" with
                | Some i ->
                  let i = Gwdb.iper_of_string i in
                    if (Adef.mother cpl) = i then
                      (Adef.mother cpl, "af")
                    else
                      let a = Gwdb.poi base i in
                      (match Gwdb.get_parents a with
                      | Some x when x = ifam -> (i, "aa")
                      | _ -> (Adef.father cpl, "af"))
                | None -> (Adef.father cpl, "af")
              in
              match act with
              | "af" ->
                  let gen_p =
                    Geneweb.Util.string_gen_person
                      base (Gwdb.gen_person_of_person (Gwdb.poi base ip))
                  in
                  (Def.U_Add_family (gen_p, fam), "af")
              | _ ->
                  let gen_p =
                    Geneweb.Util.string_gen_person
                      base (Gwdb.gen_person_of_person (Gwdb.poi base ip))
                  in
                  (Def.U_Add_parent (gen_p, fam), "aa")
            in
            let hr =
              [(fun () -> Geneweb.History.record conf base changed act);
               (fun () -> Geneweb.Update.delete_topological_sort conf base)]
            in
            Some ifam, Api_update_util.UpdateSuccess (wl, ml, hr, None)
          end)
  with
  | Geneweb.Update.ModErr s -> None, Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> None, Api_update_util.UpdateErrorConflict c)


let print_mod_aux conf base mod_f callback =
  try
    let (sfam, scpl, sdes) = reconstitute_family conf base mod_f in
      match Geneweb.UpdateFamOk.check_family conf sfam scpl sdes with
      | (Some err, _, _) | (_, Some err, _) | (_, _, Some err) ->
          (* Correspond au cas ou fn/sn = "" ou "?" *)
          (* => ne devrait pas se produire *)
          Api_update_util.UpdateError err
      | (None, None, None) ->
          let (sfam, sdes) = Geneweb.UpdateFamOk.strip_family sfam sdes in
          callback sfam scpl sdes
  with
  | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c


let print_mod conf base ip mod_f =
  let ifam = Gwdb.ifam_of_string @@ Int32.to_string mod_f.Api_saisie_write_piqi.Family.index in
  let o_f =
    Geneweb.Util.string_gen_family
      base (Gwdb.gen_family_of_family (Gwdb.foi base ifam))
  in
  let callback sfam scpl sdes =
    begin
      let ofs = Geneweb.UpdateFamOk.family_structure base sfam.Def.fam_index in
      let (ifam, fam, cpl, des) =
        Geneweb.UpdateFamOk.effective_mod conf base true sfam scpl sdes
      in
      let () = Geneweb.UpdateFamOk.patch_parent_with_pevents base cpl in
      let () = Geneweb.UpdateFamOk.patch_children_with_pevents base des in
      let s =
        let sl =
          [fam.comment; fam.fsources; fam.marriage_note; fam.marriage_src]
        in
        let rec loop l accu =
          match l with
          | [] -> accu
          | evt :: l -> loop l (evt.Def.efam_note :: evt.Def.efam_src :: accu)
        in
        let sl = loop (fam.fevents) sl in
        String.concat " " (List.map (Gwdb.sou base) sl)
      in
      Geneweb.Notes.update_notes_links_db base (Def.NLDB.PgFam ifam) s;
      let nfs = (Adef.parent_array cpl, des.children) in
      let onfs = Some (ofs, nfs) in
      let (wl, ml) =
        Geneweb.UpdateFamOk.all_checks_family
          conf base ifam fam cpl des (scpl, sdes, onfs)
      in
      let changed =
        let p =
          Geneweb.Util.string_gen_person
            base (Gwdb.gen_person_of_person (Gwdb.poi base ip))
        in
        let n_f = Geneweb.Util.string_gen_family base fam in
        Def.U_Modify_family (p, o_f, n_f)
      in
      let hr =
        [(fun () -> Geneweb.History.record conf base changed "mf");
         (fun () -> Geneweb.Update.delete_topological_sort conf base)]
      in
      Api_update_util.UpdateSuccess (wl, ml, hr, None)
    end
  in
  print_mod_aux conf base mod_f callback
