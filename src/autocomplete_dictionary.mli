module Volume : sig
  type t
  val make : process_csv_row:(string list -> string list) -> t
end

type t

val make : csv_filename:string -> (string * Volume.t) list -> t

val marshal : assets:string -> lang:string -> t -> unit

val unmarshal_volume :
  assets:string -> lang:string -> string -> string array

val volume_filename : assets:string -> lang: string -> string -> string
