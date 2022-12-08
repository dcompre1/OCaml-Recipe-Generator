(*name * id * instructions * area * img * vid * ingredients * measurements*)
type meal =  string * string * string * string * string * string * string list * string list

val get_name: meal -> string
val get_id: meal -> string 
val get_instructions: meal -> string
val get_area: meal -> string
val get_vid: meal -> string
val get_ingredients: meal -> string list 
val get_measurements: meal -> string list

val get_ordered_ingredients: meal -> string list

val get_ordered_instructions: meal -> string list

val merge_with: string list -> string list -> int ->string list

val get_video: meal -> string option

val get_img: meal -> string option
    
val print_meal: meal -> unit
      
val meal_to_file: meal -> string -> unit 