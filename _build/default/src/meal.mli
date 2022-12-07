(*name * id * instructions * area * img * vid * ingredients * measurements*)
type meal =  string * string * string * string * string * string * string list * string list
(*{name : string; id : string; instructions : string; area: string; img : string; vid : string ; ingredients : string list; measurements: string list} [@@deriving yojson] *)

(*with a meal as input , return the list of its ingredients
   example: "chicken", "cheese", "onion"*)
val get_ingredients: meal -> int -> string list

val get_instructions: meal -> string list
   
   (* return a string option of video url
      return None if there is no video associated with the meal, return Some URL_STRING if a video exists*)   
val get_video: meal -> string option
   
   (* return a string option of image url
      return None if there is no image associated with the meal, return Some URL_STRING if an image exists*)   
val get_img: meal -> string option
    
      (*this function will only be used if a frontend is not created for this project, print meal info to file*)
val print_meal: meal -> string -> unit 