type ('a, 'b) pb_person =
  | PLight of 'a
  | PFull of 'b
;;

type filters =
  { only_sosa : bool;
    only_recent : bool;
    filter_sex : Def.sex option;
    nb_results : bool;
    date_birth : (Date.dmy * Date.dmy * bool) option;
    date_death : (Date.dmy * Date.dmy * bool) option;
  }
;;
