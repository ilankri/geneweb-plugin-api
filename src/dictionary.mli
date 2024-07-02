module StrSet : Set.S

module type Store = sig
  type t
  val empty : t
end

module Make (Store : Store) : sig
  module Volume : sig
    type t = {
        name : string;
        add_entry : Store.t -> string list -> Store.t;
        get_all_entries : Store.t -> StrSet.t;
      }
  end

  type t

  val make : csv_filename:string -> Volume.t list -> t

  val marshal : assets:string -> lang:string ->  t -> unit

  val unmarshal_volume : string -> string array
end
