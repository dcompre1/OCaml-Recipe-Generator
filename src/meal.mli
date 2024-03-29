
type meal = {
  name : string;
  id : string;
  instructions : string;
  area : string;
  img : string;
  vid : string;
  ingredients : string list;
  measurements : string list;
}

val empty_meal : meal
val equal : meal -> meal -> bool
val is_empty : meal -> bool
val get_ingredients : (string * string) list -> string list
val get_measurements : (string * string) list -> string list
val lookup : string -> (string * string) list -> string

val get_meal_info :
  (string * string) list ->
  (string * string * string * string * string * string) option

val create_meal : (string * string) list -> meal option
val get_ordered_ingredients : meal -> string list
val get_ordered_instructions : meal -> string list
val merge_with : string list -> string list -> int -> string list
val get_video : meal -> string option
val get_img : meal -> string option
val print_list: string list -> unit
val print_meal : meal -> unit
val meal_to_file : meal -> string -> unit
