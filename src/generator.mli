
module M = Meal
type meal = M.meal

val get_recipe: char -> string -> string -> string list -> unit
(*val get_ingredients: (string * string) list -> string list

val get_measurements: (string * string) list -> string list 

val lookup: string -> (string * string) list -> string

val get_meal_info: (string * string) list -> (string * string * string * string * string * string) option
*)
val find_ingredients: string -> string list -> bool

(* make API request based on char and string values and return meal list: Some [meal1, meal2 ... ]
   if char == v: use url for finding a vegan meal www.themealdb.com/api/json/v1/1/filter.php?c=Vegan
    if char == g: use url for finding a vegetarian meal www.themealdb.com/api/json/v1/1/filter.php?c=Vegetarian

    if char == c: use string with category url www.themealdb.com/api/json/v1/1/filter.php?c=[cat. Capitalized]
    if char == i: use string with ingredient url www.themealdb.com/api/json/v1/1/filter.php?i=[ingredient lowercase and underscores no spaces]
    if char == a: use string with areas url www.themealdb.com/api/json/v1/1/filter.php?a=[area Capitalized]

    return None if no meals returned from API request *)
val find_meals: string -> meal list

(* sanitize input string and return a list of possible ingredients to avoid that include this string

   - all non space/non alpha characters dropped
   - capitalize each word
   
   examples: 
      "cheese" would return ["Cheddar Cheese"; "Swiss Cheese"; ...]
      "Chicken" would return ["Chicken"; "Chicken Breast"; "Chicken Stock"; ...]  

   possibly include an algorithm that can take care of misspelled words *)
(*val find_ingredients: string -> string list -> string list *)


(* for all of the strings in the list, make a call to format_ingredient and replace/fix each string 
   return the corrected list of strings
   separate string by whitespace and then call format_ingredient on each value in this list*)
val format_restrictions: string list -> string list 

(* ingredients need to be in a specific format to use with url
   - Spaces changed into underscores
   - all other whitespace/non alpha characters dropped
   - all lowercase

   ex: "Peanut Butter" -> "peanut_butter"
*)
val format_ingredient: string -> string

(* simple helper method that will compare two lists and return true if they have any elements in common *)
val has_restrictions: string list -> string list -> bool

(* take in a list of meals
   get_ingredients of each meal and call has_restrictions on a provided list of restrictions, do not return any meals that contain user's restricted ingredients *)
val filter_meals: string list -> meal list -> meal list

(*make an API request to: www.themealdb.com/api/json/v1/1/random.php, save information as meal *)
val get_random_meal: string ->  unit

(* return a randomly selected meal from a list of meals *)
val get_meal: meal list -> meal

(* string is the ID value
   call valid_id
   use url to find a meal by its ID:  www.themealdb.com/api/json/v1/1/lookup.php?i=52772 and return the meal*)
val get_meal_by_id: string -> int -> unit

(* figure out if the ID provided only contains digits return false or true 
   if false, another function will have to handle this input issue/display an error to user *)
val valid_id: string -> bool

(* format a string of a meal name to work with url
   - separate the string by whitespace
   - capitalize each word
   - put the string backtogether with underscores instead of spaces
   return this newly formatted string *)
val format_meal_name: string -> string

(* make call to format_meal_name
   use url for finding a meal by its name:  www.themealdb.com/api/json/v1/1/search.php?s=Arrabiata *)
val get_meal_by_name: string -> string -> unit

val print_cuisines: unit -> unit

(*potential frontend goals:
   
- welcome message to site explaining purpose and variety of options
- option 1: get a random recipe
- option 2: enter a cuisine type (indian, chinese etc.) and get a meal from this area
     - have option to enter list of allergens, unfavorable ingredients etc.,  will receive a meal that doesn't include these ingredients
- option 3: enter the ID of a meal you've seen before and get the meal associated with it 
- option 4: enter a meal name you've received before and get the meal associated with it
- option 5: get a vegan or vegetarian meal

give them a button that will lead them to a list of cuisines they can choose from 

when a meal is displayed: 
- alongside instructions, ingredients etc. display the image of the meal or generic image if none is available for that meal 
- display a vid of the meal tutorial or generic image if none is available for that meal  
- have a button that allows them to prompt for another meal with the same options they choses
- a back button that will just take them back to the choosing screen
- indicate that they should save the meal ID or meal name if they enjoyed the meal and want to search it again on the site 

*)
