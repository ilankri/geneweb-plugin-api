module M = Api_piqi
module Mext = Api_piqi_ext

open Geneweb
open Config
open Gwdb
open Def
open Util
open Api_def
open Api_util

module StrSet = Mutil.StrSet

let string_start_with ini s = Mutil.start_with_wildcard ini 0 s

(* Algo de Knuth-Morris-Pratt *)
let init_next p =
  let m = String.length p in
  let next = Array.make m 0 in
  let i = ref 1 and j = ref 0 in
  while !i < m - 1 do
    if p.[!i] = p.[!j] then begin incr i; incr j; next.(!i) <- !j end
    else if !j = 0 then begin incr i; next.(!i) <- 0 end
    else j := next.(!j)
  done;
  next

(* Algo de Knuth-Morris-Pratt *)
let kmp p s =
  let p = (Name.lower p) in
  let s = (Name.lower s) in
  (* Optimisation pour GeneWeb *)
  if s = "" || s = "?" then false
  else
    begin
      let next = init_next p and m = String.length p in
      let n = String.length s and i = ref 0 and j = ref 0 in
      while !j < m && !i < n do
        if s.[!i] = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.(!j)
      done;
      if !j >= m then true else false
    end

(* FIXME: DUPLICATE OF ALLN.SELECT ??? *)
(* ************************************************************************** *)
(*  [Fonc] get_list_of_select_start_with :
    config -> base -> bool -> string -> bool -> name -> letter                *)
(** [Description] : Fonction qui scanne l'ensemble des noms de la base
    pour une lettre donnée et retourne une liste de personne.
    [Args] :
      - conf            : configuration de la base
      - base            : base de donnée
      - is_surnames     : si True recherche sur les noms de famille
      - ini_p           : début du nom
      - ini_n           : début du prénom
      - need_whole_list : si True, remonte tout
      - letter          : la première lettre des noms/prénoms concernés
    [Retour] :
      - ListPersons : Retourne une liste de personnes.
                                                                              *)
(* ************************************************************************** *)
let get_list_of_select_start_with conf base ini_n ini_p letter =
    let name =
    (* Si le nom est défini, on parcourt un tableau de noms *)
    if "" <> ini_n
    then
        persons_of_surname base
    else
        (* Sinon, on parcourt un tableau de prénoms *)
        persons_of_first_name base
    in
    try
      (* Itère sur chaque entrée du tableau qui commence par la lettre letter *)
      let istr = spi_first name letter in
        let rec loop istr list =
          let s = Mutil.nominative (sou base istr) in
          let k = Util.name_key base s in
            (* Vérifie que le début du nom de famille de la personne correspond à celui demandé *)
            if "" = ini_n || string_start_with (Name.lower ini_n) (Name.lower k) then
              let list =
                if s <> "?" then
                  let my_list = spi_find name istr in
                  let my_list =
                    List.fold_left
                     (fun l ip ->
                        let p = poi base ip in
                        let isn = get_surname p in
                        if "" <> ini_n
                        then
                            if eq_istr isn istr then
                                if "" <> ini_p
                                then
                                    let isp = sou base (get_first_name p) in
                                    if eq_istr isn istr && string_start_with (Name.lower ini_p) (Name.lower isp)
                                    then
                                        (* Prénom===Prénom && Nom===Nom *)
                                        (ip :: l)
                                    else l
                                else
                                    (* Prénom===* && Nom===Nom *)
                                    (ip :: l)
                            else l
                        else
                            if "" <> ini_p
                            then
                                let isp = sou base (get_first_name p) in
                                if string_start_with (Name.lower ini_p) (Name.lower isp)
                                then
                                    (* Prénom===Prénom && Nom===* *)
                                    (ip :: l)
                                else l
                            else
                                (* Prénom===* && Nom===* *)
                                (ip :: l)
                        )
                     [] my_list
                  in
                  let my_list =
                    if conf.use_restrict then
                      List.fold_left
                        (fun l ip ->
                          if is_restricted conf base ip then l
                           else (ip :: l) )
                        [] my_list
                    else my_list
                  in
                  (* Ajoute à list les personnes trouvées *)
                  List.rev_append my_list list
                (* Sort totalement de l'itération puisque les personnes ne sont plus définies *)
                else list
              in
              match spi_next name istr with
              | istr -> loop istr list
              | exception Not_found -> list
            else
              match spi_next name istr with
              | istr -> loop istr list
              | exception Not_found -> list
        (* Première itération, on initialise au passage list comme tableau vide *)
        in loop istr []
    with Not_found -> []

(* ************************************************************************** *)
(*  [Fonc] select_start_with :
    config -> base -> string -> string -> bool                                  *)
(** [Description] : Retourne une liste de personne dont le nom OU le prénom
    commence par 'ini_n' ou 'ini_p'.
    [Args] :
      - conf            : configuration de la base
      - base            : base de donnée
      - ini_n           : début du nom à chercher
      - ini_p           : début du nom à chercher
      - need_whole_list : si True, remonte tout
    [Retour] :
      - ListPersons : Retourne une liste de personnes.
                                                                              *)
(* ************************************************************************** *)
let select_start_with conf base ini_n ini_p =
  let ini_n = Util.name_key base ini_n in
  let start =
    if ini_n <> ""
    then
      Mutil.tr '_' ' ' ini_n
    else
      Mutil.tr '_' ' ' ini_p
  in
  let list_min =
    let letter = String.lowercase_ascii (String.sub start 0 1) in
    get_list_of_select_start_with conf base ini_n ini_p letter
  in
  let list_maj =
    let letter = String.uppercase_ascii (String.sub start 0 1) in
    get_list_of_select_start_with conf base ini_n ini_p letter
  in
  List.rev_append list_maj list_min

let aux_ini (s : string) : string list =
  let rec loop (s : Adef.encoded_string) acc =
    if String.contains (s :> string) '+' then
      let index = String.index (s :> string) '+' in
      let start = index + 1 in
      let len = String.length (s :> string) - start in
      let ns = Adef.encoded @@ String.sub (s :> string) start len in
      loop ns (Mutil.decode (Adef.encoded @@ String.sub (s :> string) 0 index) :: acc)
    else (Mutil.decode s :: acc)
  in
  loop (Mutil.encode s) []

let select_both_all base ini_n ini_p maiden_name =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let find_str s x = kmp x s in
  let ini_n = Util.name_key base ini_n in
  let ini_n = aux_ini ini_n in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = aux_ini ini_p in
  let add_maiden p ini_p l =
    if get_sex p = Male then
      l :=
        Array.fold_left
          (fun acc ifam ->
             let fam = foi base ifam in
             let ip = Gutil.spouse (get_iper p) fam in
             let sp = poi base ip in
             if List.for_all (fun s -> find_fn sp s) ini_p then ip :: acc else acc)
          !l (get_family p)
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> List.rev_append (String.split_on_char '-' s) accu)
        [] ini_n
    in
    if get_sex p = Male && List.exists (fun s -> find_sn p s) ini_n
    then
      Array.iter
        (fun ifam ->
           let fam = foi base ifam in
           let ip = Gutil.spouse (get_iper p) fam in
           let sp = poi base ip in
           let names =
             sou base (get_surname p) ^ " " ^ sou base (get_surname sp)
           in
           if List.for_all (fun s -> find_str names s) ini_n
           then add_maiden p ini_p l)
        (get_family p)
  in
  let list = ref [] in
  Gwdb.Collection.iter begin fun p ->
    let ip = get_iper p in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  end (Gwdb.persons base) ;
  !list

let select_all base is_surnames ini =
  let find p x =
    if is_surnames then kmp x (sou base (get_surname p))
    else kmp x (sou base (get_first_name p))
  in
  let ini =
    if is_surnames then Util.name_key base ini
    else ini
  in
  let ini = aux_ini ini in
  let list = ref [] in
  Gwdb.Collection.iter begin fun p ->
    if List.for_all (fun s -> find p s) ini
    then list := get_iper p :: !list
  end (Gwdb.persons base) ;
  !list

module PersonSet = Set.Make (struct
    type t = Gwdb.person
    let compare i1 i2 = Stdlib.compare (get_iper i1) (get_iper i2)
  end)

let print_list conf base filters list =
  let person_l =
    PersonSet.elements
      (List.fold_left
         begin fun acc p ->
           let p = poi base p in
           if apply_filters_p conf filters SosaCache.get_sosa_person p
           then PersonSet.add p acc
           else acc
         end
         PersonSet.empty list)
  in
  let person_l =
    if filters.nb_results then person_l
    else
      List.sort
        (fun p1 p2 ->
          let sn1 = Name.lower (p_surname base p1) in
          let sn2 = Name.lower (p_surname base p2) in
          let comp = Gutil.alphabetic_order sn1 sn2 in
          if comp = 0 then
            let fn1 = Name.lower (p_first_name base p1) in
            let fn2 = Name.lower (p_first_name base p2) in
            Gutil.alphabetic_order fn1 fn2
          else comp)
        person_l
  in
  let data = conv_data_list_person conf base filters person_l in
  print_result conf data

(*
   La différence entre la recherche approximative et lastname_or_surname est
   si on cherche un nom ET un prénom (dans les autres cas, on obtient les
   mêmes résultats.
   De ce fait, on utilise list_n = select_all n, list_p = select_all p et on
   fait l'union des deux ce qui est beaucoup plus efficace.
*)
let print_search conf base =
  let search_params = get_params conf (fun x f -> Mext.parse_search_params x f) in
  let filters = get_filters conf in
  match
     (search_params.M.Search_params.lastname,
      search_params.M.Search_params.firstname)
  with
   | (Some n, Some fs) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower n = "" && Name.lower fs = "" then
          []
        else
          let maiden_name = search_params.M.Search_params.maiden_name in
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base n fs
          | `approximative -> select_both_all base n fs maiden_name
          | `lastname_or_firstname ->
               let list_n = select_all base true n in
               let list_p = select_all base false fs in
               List.rev_append list_n list_p
      in
      print_list conf base filters list
  | (Some n, None) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower n = "" then
          []
        else
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base n ""
          | `approximative -> select_all base true n
          | `lastname_or_firstname -> select_all base true n
      in
      print_list conf base filters list
  | (None, Some fs) ->
      let _ = load_strings_array base in
      let list =
        if Name.lower fs = "" then
          []
        else
          match search_params.M.Search_params.search_type with
          | `starting_with -> select_start_with conf base "" fs
          | `approximative -> select_all base false fs
          | `lastname_or_firstname -> select_all base false fs
      in
      print_list conf base filters list
  | (None, None) -> ()



(**/**) (* Recherche utilisée pour l'auto-completion ou relier personne. *)

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i

let string_incl_start_with x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then true
        else if j = String.length y then
          if x.[i] = '_' then loop1 (i + 1) j
          else false
        else if y.[j] = x.[i] || y.[j] = ' ' && x.[i] = '_' then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0

let select_both_start_with_person base ini_n ini_p =
  let find n x = string_start_with x n in
  let ini_n = aux_ini (Name.lower ini_n) in
  let ini_p = aux_ini (Name.lower ini_p) in
  Gwdb.Collection.fold begin fun list p ->
      let surnames = aux_ini (Name.lower (sou base (get_surname p))) in
      let first_names = aux_ini (Name.lower (sou base (get_first_name p))) in
      let start_surname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) surnames)
          ini_n
      in
      let start_firstname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) first_names)
          ini_p
      in
      if start_surname && start_firstname then (get_iper p :: list)
      else list
  end [] (Gwdb.persons base)

let select_start_with_person base get_field ini =
  let find n x = string_start_with x n in
  let ini = aux_ini (Name.lower ini) in
  Gwdb.Collection.fold begin fun list p ->
      let names = aux_ini (Name.lower (sou base (get_field p))) in
      let start_name =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) names)
          ini
      in
      if start_name then (get_iper p :: list)
      else list
  end [] (Gwdb.persons base)

let matching_nameset base stop max_res istr name_f name first_letter =
  let rec aux n istr set =
    let s = sou base istr in
    let k = Util.name_key base s in
    if n < max_res && (stop && String.sub k 0 1 = first_letter || not stop) then
      let n, set =
        if string_incl_start_with (Name.lower name) (Name.lower k) then
          n + 1, StrSet.add s set
        else n, set
      in
      match spi_next name_f istr with
      | exception Not_found -> n, set
      | istr -> aux n istr set
    else n, set
  in
  aux 0 istr StrSet.empty

let matching_nameset' base stop max_res name_f name first_letter =
  match spi_first name_f first_letter with
  | exception Not_found -> 0, StrSet.empty
  | istr -> matching_nameset base stop max_res istr name_f name first_letter

let matching_nameset_of_input base stop uppercase name_f max_res name =
  let name' = Mutil.tr '_' ' ' name in
  let first_letter = String.sub name' 0 1 in
  let first_letter =
    if uppercase then String.uppercase_ascii first_letter
    else String.lowercase_ascii first_letter
 in
 matching_nameset' base stop max_res name_f name first_letter

let select_start_with_auto_complete base mode max_res input =
  let name_f =
    match mode with
    | `lastname -> persons_of_surname base
    | `firstname -> persons_of_first_name base
    | `place -> failwith "cannot use select_start_with_auto_complete"
    | `source -> failwith "cannot use select_start_with_auto_complete"
  in
  (* Si la base est grosse > 100 000, on fait un vrai start_with. *)
  if Gwdb.nb_of_persons base > 100000 then
    begin
      let name =
        match mode with
        | `lastname -> Util.name_key base input
        | `firstname -> input
        | `place -> failwith "cannot use select_start_with_auto_complete"
        | `source -> failwith "cannot use select_start_with_auto_complete"
      in
      (* uppercase *)
      let nb_res, maj_set = matching_nameset_of_input base true true name_f max_res name in
      (* lowercase *)
      let _, min_set = matching_nameset_of_input base true false name_f (max_res - nb_res) name in
      StrSet.union maj_set min_set
    end
  else
    begin
      (* On commence à ? comme ça on fait MAJ et MIN. *)
      let starting_letter = "\000" in
      snd @@ matching_nameset' base false max_res name_f input starting_letter
    end

let select_start_with_auto_complete base mode max_res ini =
  let s = select_start_with_auto_complete base mode max_res ini in
  let l = StrSet.elements s in
  List.sort Gutil.alphabetic_order l

let select_all_auto_complete _ base get_field max_res ini =
  let find p x = kmp x (sou base (get_field p)) in
  let ini = aux_ini ini in
  let string_set = ref StrSet.empty in
  let nb_res = ref 0 in
  Gwdb.Collection.fold_until (fun () -> !nb_res < max_res) begin fun () p ->
      if List.for_all (fun s -> find p s) ini
      then
        begin
        string_set := StrSet.add (sou base (get_field p)) !string_set;
        incr nb_res;
        end
  end () (Gwdb.persons base) ;
  List.sort Gutil.alphabetic_order (StrSet.elements !string_set)

let get_field mode =
  match mode with
  | `lastname -> get_surname
  | `firstname -> get_first_name
  | _ -> failwith "get_field"

type dico = string array

let dico_fname assets lang k =
  Option.map (Filename.concat assets) @@ match k with
  | `town -> Some ("dico.town." ^ lang ^ ".bin~")
  | `area_code -> Some ("dico.area_code." ^ lang ^ ".bin~")
  | `county -> Some ("dico.county." ^ lang ^ ".bin~")
  | `region -> Some ("dico.region." ^ lang ^ ".bin~")
  | `country -> Some ("dico.country." ^ lang ^ ".bin~")
  | `subdivision -> None

(** [ini] must be in the form of [Name.lower @@ Mutil.tr '_' ' ' ini]
    Assume that [list] is already sorted, but reversed.
*)
let complete_with_dico assets conf nb max mode ini list =
  let reduce_dico mode ignored format list =
    let len = Array.length list in
    let rec loop acc i =
      if i = len
      then acc
      else
        let hd = Array.unsafe_get list i in
        let acc =
          let k =  Mutil.tr '_' ' ' hd in
          let k = if mode <> `subdivision then Place.without_suburb k else k in
          if string_start_with ini (Name.lower k) then begin
            let hd =
              if format <> []
              then
                let expl_hd = Api_csv.row_of_string hd in
                String.concat ", " @@
                Mutil.filter_map begin function
                  | `town -> List.nth_opt expl_hd 0
                  | `area_code -> List.nth_opt expl_hd 1
                  | `county -> List.nth_opt expl_hd 2
                  | `region -> List.nth_opt expl_hd 3
                  | `country -> List.nth_opt expl_hd 4
                  | _ -> None
                end
                  format
              else
                hd
            in
            if List.mem hd ignored then acc
            else begin incr nb ; hd :: acc end
          end
          else acc
        in
        if !nb < max then loop acc (i + 1) else acc
    in loop [] 0
  in
  match mode with
  | Some mode when !nb < max ->
    let format =
      match List.assoc_opt "places_format" conf.base_env with
      | None -> []
      | Some s ->
        List.map begin function
          | "Subdivision" -> `subdivision
          | "Town" -> `town
          | "Area code" -> `area_code
          | "County" -> `county
          | "Region" -> `region
          | "Country" -> `country
          | _ -> raise Not_found
          end
          (Api_csv.row_of_string s)
    in
    let dico =
      begin match dico_fname assets conf.lang mode with
        | Some fn -> Mutil.read_or_create_value fn (fun () : dico -> [||])
        | None -> [||]
      end |> reduce_dico mode list format
    in
    List.rev_append list (List.sort Place.compare_places dico)
  | _ -> List.rev list

let search_auto_complete assets conf base mode place_mode max n =
  let aux data compare =
    let conf = { conf with env = ("data", Mutil.encode data) :: conf.env } in
    UpdateData.get_all_data conf base
    |> List.rev_map (sou base)
    |> List.sort compare
  in
  match mode with

  | `place ->
    let list = aux "place" Place.compare_places in
    let nb = ref 0 in
    let ini = Name.lower @@ Mutil.tr '_' ' ' n in
    let reduce_perso list =
      let rec loop acc = function
        | [] -> acc
        | hd :: tl ->
          let hd' =
            if place_mode <> Some `subdivision
            then Place.without_suburb hd
            else hd
          in
          let acc =
            if Mutil.start_with_wildcard ini 0 @@ Name.lower @@ Mutil.tr '_' ' ' hd'
            then (incr nb ; hd :: acc)
            else acc
          in
          if !nb < max then loop acc tl else acc
      in
      loop [] list
    in
    complete_with_dico assets conf nb max place_mode ini (reduce_perso list)

  | `source ->
    let list = aux "src" Gutil.alphabetic_order in
    let nb = ref 0 in
    let ini = Name.lower @@ Mutil.tr '_' ' ' n in
    let rec reduce acc = function
      | [] -> List.rev acc
      | hd :: tl ->
        let k =  Mutil.tr '_' ' ' hd in
        let acc =
          if string_start_with ini (Name.lower k)
            then (incr nb ; hd :: acc)
            else acc
        in
        if !nb < max then reduce acc tl
        else List.rev acc
    in
    reduce [] list

  | _ ->
    if Name.lower n = "" then []
    else ( load_strings_array base
         ; select_start_with_auto_complete base mode max n )

let select_both_link_person base ini_n ini_p max_res =
  let find_sn p x = kmp x (sou base (get_surname p)) in
  let find_fn p x = kmp x (sou base (get_first_name p)) in
  let ini_n = Util.name_key base ini_n in
  let ini_n = aux_ini ini_n in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = aux_ini ini_p in
  fst @@ Gwdb.Collection.fold_until (fun (_, n) -> n < max_res) begin fun (list, n) p ->
    if List.for_all (fun s -> find_sn p s) ini_n then
      if List.for_all (fun s -> find_fn p s) ini_p then
        (get_iper p :: list, n + 1)
      else (list, n)
    else (list, n)
  end ([], 0) (Gwdb.persons base)

let select_link_person base get_field max_res ini =
  let find p x = kmp x (sou base (get_field p)) in
  let ini = aux_ini ini in
  fst @@ Gwdb.Collection.fold_until (fun (_, n) -> n < max_res) begin fun (list, n) p ->
      if List.for_all (fun s -> find p s) ini
      then (get_iper p :: list, n + 1)
      else (list, n)
  end ([], 0) (Gwdb.persons base)

let search_person_list base surname first_name =
  let _ = load_strings_array base in
  let (surname, first_name) =
    match (surname, first_name) with
    | (Some n, Some fn) ->
        ((if Name.lower n = "" then None else Some n),
         (if Name.lower fn = "" then None else Some fn))
    | (Some n, None) -> ((if n = "" then None else Some n), None)
    | (None, Some fn) -> (None, (if fn = "" then None else Some fn))
    | (None, None) -> (None, None)
  in
  match (surname, first_name) with
  | (Some n, Some fn) ->
      select_both_start_with_person base n fn
  | (Some n, None) ->
      select_start_with_person base get_surname n
  | (None, Some fn) ->
      select_start_with_person base get_first_name fn
  | (None, None) -> []
