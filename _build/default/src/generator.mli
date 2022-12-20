module M = Meal

type meal = M.meal

val tuple_list: string list -> (string * string) list
val find_ingredients : string -> string list -> bool
val format_ingredient : string -> string
val format_restrictions : string list -> string list
val has_restrictions : string list -> string list -> bool
val filter_meals : string list -> meal list -> meal list
val valid_id : string -> bool
val format_meal_name : string -> string

module type Randomness = sig
  val int : int -> int
end

val get_meal : (module Randomness) -> meal list -> meal
val rem_meal : meal -> meal list -> meal list
val format_file : string -> string -> string
val no_meals : meal list -> bool
val find_meal : int -> meal list -> meal
val number_list : meal list -> string list
val find_meals : string -> meal list
val get_random_meal : string -> unit
val print_meal_by_name : string -> string -> unit
val print_meal_by_id : string -> string -> unit
val print_recipe : char -> string -> string -> string list -> unit
val print_recipe_list : char -> string -> string -> string list -> unit
val print_cuisines : unit -> unit
