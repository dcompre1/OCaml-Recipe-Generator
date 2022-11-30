(*implementation of functions in generator.mli*)
open Core

type meal = { name : string; id : int; instructions : string; area: string; ingredients : string list; img : string; vid : string } [@@deriving yojson]

[@@@ocaml.warning "-27"]
let unimplemented () =
	failwith "unimplemented"
	
(* getter functions for values needed in later comparisons 
   get_video and get_img will especially help for frontend implementation alternative images will need to be displayed if these not provided from TheMealDB *)

(*with a meal as input , return the list of its ingredients
   example: "chicken", "cheese", "onion"*)
let get_ingredients(m: meal): string list = 
   unimplemented ()

(*with a meal as input, return the string that indicates the area the meal originates from
   examples: "Indian", "Chinese", "Mexican"*)
let get_area (m: meal): string = 
   unimplemented ()

(* return a string option of video url form meal type 
   return None if there is no video associated with the meal, return Some URL_STRING if a video exists*)   
let get_video (m: meal): string option = 
   unimplemented ()

(* return a string option of image url form meal type 
   return None if there is no image associated with the meal, return Some URL_STRING if an image exists*)   
let get_img (m: meal): string option = 
   unimplemented ()

(* make API request based on char and string values and return meal list: Some [meal1, meal2 ... ]
   if char == v: use url for finding a vegan meal www.themealdb.com/api/json/v1/1/filter.php?c=Vegan
    if char == g: use url for finding a vegetarian meal www.themealdb.com/api/json/v1/1/filter.php?c=Vegetarian

    if char == c: use string with category url www.themealdb.com/api/json/v1/1/filter.php?c=[cat. Capitalized]
    if char == i: use string with ingredient url www.themealdb.com/api/json/v1/1/filter.php?i=[ingredient lowercase and underscores no spaces]
    if char == a: use string with areas url www.themealdb.com/api/json/v1/1/filter.php?a=[area Capitalized]

    return None if no meals returned from API request *)
let find_meals (c: char) (s: string): meal list option =
   unimplemented ()

(* sanitize input string and return a list of possible ingredients to avoid that include this string

   - all non space/non alpha characters dropped
   - capitalize each word
   
   examples: 
      "cheese" would return ["Cheddar Cheese"; "Swiss Cheese"; ...]
      "Chicken" would return ["Chicken"; "Chicken Breast"; "Chicken Stock"; ...]  

   possibly include an algorithm that can take care of misspelled words, this likely won't be implemented unless I find I have a lot of extra time *)
let find_ingredients (ingredient: string): string list =
   unimplemented ()
(*
(* for all of the strings in the list, make a call to format_ingredient and replace/fix each string 
   return the corrected list of strings
   separate string by whitespace and then call format_ingredient on each value in this list*)
val format_restrictions: string list -> string list *)

(* ingredients need to be in a specific format to use with url
   - Spaces changed into underscores
   - all other whitespace/non alpha characters dropped
   - all lowercase

   ex: "Peanut Butter" -> "peanut_butter"
*)
let format_ingredient (ingredient: string): string = 
   unimplemented ()

(* simple helper method that will compare two lists and return true if they have any elements in common *)
let has_restrictions (ingredients: string list) (restrictions: string list): bool = 
   unimplemented ()

(* take in a list of meals
   get_ingredients of each meal and call has_restrictions on a provided list of restrictions, do not return any meals that contain user's restricted ingredients *)
let filter_meals (restrictions: string list) (m: meal list): meal list = 
   unimplemented ()

(*make an API request to: www.themealdb.com/api/json/v1/1/random.php, save information as meal *)
let get_random_meal (filename: string): unit = 
   unimplemented ()

(* return a randomly selected meal from a list of meals *)
let get_meal (m: meal list): meal = 
   unimplemented ()

(* Call find_meals to get all vegan meals, call filter_meals with string list provided
   From this list call get_meal to get one random meal and return this with the rest of the list without this meal in it *)
let get_vegan_recipe (restrictions: string list): meal * meal list = 
   unimplemented ()

(* Call find_meals to get all vegetarian meals, call filter_meals with string list provided
   From this list call get_meal to get one random meal and return this with the rest of the list without this meal in it *)
let get_vegetarian_recipe (restrictions: string list): meal * meal list = 
   unimplemented ()

(* Call format_ingredient
   Call find_meals to get all meals with this ingredient, call filter_meals with string list provided
   From this list call get_meal to get one random meal and return this with the rest of the list without this meal in it *)
let get_ingredient_recipe (ingredient: string) (restrictions: string list): meal * meal list = 
   unimplemented ()

(* string is the ID value
   call valid_id
   use url to find a meal by its ID:  www.themealdb.com/api/json/v1/1/lookup.php?i=52772 and return the meal*)
let get_meal_by_id (id: string): meal = 
   unimplemented ()

(* figure out if the ID provided only contains digits return false or true 
   if false, another function will have to handle this input issue/display an error to user *)
let valid_id (id: string): bool = 
   unimplemented ()

(* format a string of a meal name to work with url
   - separate the string by whitespace
   - capitalize each word
   - put the string backtogether with underscores instead of spaces
   return this newly formatted string *)
let format_meal_name (name: string): string = 
   unimplemented ()

(* make call to format_meal_name
   use url for finding a meal by its name:  www.themealdb.com/api/json/v1/1/search.php?s=Arrabiata *)
let get_meal_by_name (name: string): meal = 
   unimplemented ()

(*this function will only be used if a frontend is not created for this project, print meal info to file*)
let print_meal (m: meal) (filename: string): unit =  
   unimplemented ()

(*this function will only be used if a frontend is not created for this project, print all area options to stdout*)
let print_cuisines (): unit = 
   unimplemented ()

