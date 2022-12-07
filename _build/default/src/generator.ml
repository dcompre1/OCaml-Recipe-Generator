(*implementation of functions in generator.mli*)
open Core
(*open Cohttp *)
open Lwt
open Cohttp_lwt_unix
(*open Str *)
module M = Meal

[@@@ocaml.warning "-27"]

type meal = M.meal

let print_list (l : string list) : unit =
   List.iter ~f:(fun x -> Stdio.printf "%s\n\n " x) l

let print_tup_list(l: (string * string) list): unit = 
   List.iter ~f:(fun (x, y) -> Stdio.printf " (%s; %s) " x y) l
(*let print_s_list (l: (string * string list) list): unit =
   List.iter ~f:(fun (a,b) -> Stdio.printf "\n\n\n%s\n\n\n " a; print_list b) l
*)
(*Cohttp_lwt.Body.to_form: t -> (string * string list) list*)

(*change between >>= and >|= to solve problems lmao*)
let body (uri: string)= 
   Client.get (Uri.of_string uri) >>= fun (_, body) ->
   body |> Cohttp_lwt.Body.to_string >|= fun body -> String.drop_suffix (String.drop_prefix body 11) 3|> String.filter ~f:(fun c -> Char.(<>) '\"' c) (*|> Cohttp_lwt.Body.of_string |> Cohttp_lwt.Body.to_form >|= fun body -> body*)

let format_body (s: string): string list = 
   (*Str.split (Str.regexp "[,(?=[ ])]+") s *)
   String.substr_replace_all ~pattern:", " ~with_:"|" s|> String.split ~on:',' |> List.map ~f:( fun s -> String.substr_replace_all ~pattern:"|" ~with_:", " s)

let tuple_list (l: string list): (string * string) list = 
   List.map ~f:(fun s -> match String.lsplit2 ~on:':' s with |Some x -> x |None -> "","") l

let rand_meal (s: string) = 
   let body = Lwt_main.run (body "https://www.themealdb.com/api/json/v1/1/random.php") in
   print_tup_list ( format_body body |> tuple_list); 
   print_endline s

let rec get_ingredients (l: (string * string) list): string list = 
   match l with 
   | [] -> []
   | (key, value) :: xs -> if String.is_prefix ~prefix:"strIngred" key then value :: get_ingredients xs else get_ingredients xs

let rec get_measurements (l: (string * string) list): string list = 
   match l with 
   | [] -> []
   | (key, value) :: xs -> if String.is_prefix ~prefix:"strMeasure" key then value :: get_measurements xs else get_measurements xs

let rec lookup (k : string) (m : (string * string) list): string option =
   match m with
   | [] -> None
   | (k2, v) :: vs -> if String.compare k k2 <> 0 then lookup k vs else Some v

let get_meal_info (l: (string * string) list): string list = 
   match l with 
   | [] -> []
   | l -> List.map ~f:(fun s -> match s with |None -> "" |Some x -> x) [(lookup "idMeal" l); (lookup "strMeal" l); (lookup "strIngredients" l); (lookup "strArea" l); (lookup "strMealThumb" l); (lookup "strYoutube" l)]

(*TODO: create a meal from get_ingredients, get_measurements, get_meal_info functions
let create_meal (): meal = 
*)

let unimplemented () =
	failwith "unimplemented"
	
(* make API request based on char and string values and return meal list: Some [meal1, meal2 ... ]
   if char == v: use url for finding a vegan meal www.themealdb.com/api/json/v1/1/filter.php?c=Vegan
    if char == g: use url for finding a vegetarian meal www.themealdb.com/api/json/v1/1/filter.php?c=Vegetarian

    if char == c: use string with category url www.themealdb.com/api/json/v1/1/filter.php?c=[cat. Capitalized]
    if char == i: use string with ingredient url www.themealdb.com/api/json/v1/1/filter.php?i=[ingredient lowercase and underscores no spaces]
    if char == a: use string with areas url www.themealdb.com/api/json/v1/1/filter.php?a=[area Capitalized]

    return None if no meals returned from API request *)
let find_meals (c: char) (s: string): meal list option =
   unimplemented ()
   (*make call to API, get a series of lists into one big list, call create_meal on each meal in that list*)

(* sanitize input string and return a list of possible ingredients to avoid that include this string

   - all non space/non alpha characters dropped
   - capitalize each word
   
   examples: 
      "cheese" would return ["Cheddar Cheese"; "Swiss Cheese"; ...]
      "Chicken" would return ["Chicken"; "Chicken Breast"; "Chicken Stock"; ...]  

   possibly include an algorithm that can take care of misspelled words, this likely won't be implemented unless I find I have a lot of extra time *)

(*TODO: NVM in has_restrictions with a list of ingredients and a list of restrictions, call this function on the ingredient list and each restriction in the restriction list*)
(*let rec find_ingredients (i: string)(ingredients: string list): string list =
   match ingredients with 
   | [] -> []
   | x :: xs -> if String.is_substring ~substring:i x then x :: find_ingredients i xs else find_ingredients i xs *)
   let rec find_ingredients (i: string)(ingredients: string list): bool =
      match ingredients with 
      | [] -> false
      | x :: xs -> if String.is_substring ~substring:i x then true else find_ingredients i xs 
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
   String.substr_replace_all ~pattern:" " ~with_:"_" ingredient |> String.lowercase 

(* simple helper method that will compare two lists and return true if they have any elements in common *)
let rec has_restrictions (ingredients: string list) (restrictions: string list): bool = 
   match ingredients with 
   |[] -> false
   | l -> match restrictions with 
         | [] -> false 
         | x :: xs -> if find_ingredients x l then true else has_restrictions l xs

(* take in a list of meals
   get_ingredients of each meal and call has_restrictions on a provided list of restrictions, do not return any meals that contain user's restricted ingredients *)
let filter_meals (restrictions: string list) (m: meal list): meal list = 
  (* match m with 
   | [] -> []
   | x :: xs -> if has_restrictions x.ingredients restrictions then x :: filter_meals restrictions xs else filter_meals restrictions xs 
*) unimplemented ()

(*make an API request to: www.themealdb.com/api/json/v1/1/random.php, save information as meal *)
(*make same as rand_meal above*)
let get_random_meal (filename: string): unit = 
   unimplemented ()

(* return a randomly selected meal from a list of meals *)
(*TODO: figure out what to return in this if the list is empty, what would an "empty" meal look like?*)
(*TODO: make a function for getting nth meal from meal list*)
let get_meal (m: meal list): meal = 
   (*match m with 
   | [] -> []
   | m -> let n = Random.int ((List.length m) - 1) in List.nth_exn m n *)
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
   String.for_all ~f:(fun c -> Char.is_digit c) id && (String.length id  = 5)

(* format a string of a meal name to work with url
   - separate the string by whitespace
   - capitalize each word
   - put the string backtogether with underscores instead of spaces
   return this newly formatted string *)
let format_meal_name (name: string): string = 
   String.split_on_chars ~on:[' ';'\n';'\t';'\r'] name |> List.map ~f:(fun s -> String.capitalize s) |> List.to_string ~f:(fun s -> s ^ "_")

(* make call to format_meal_name
   use url for finding a meal by its name:  www.themealdb.com/api/json/v1/1/search.php?s=Arrabiata *)
let get_meal_by_name (name: string): meal = 
   unimplemented ()


(*this function will only be used if a frontend is not created for this project, print all area options to stdout*)
let print_cuisines (): unit = 
   let areas = ["American"; "British"; "Canadian"; "Chinese"; "Croatian"; "Dutch"; "Egyptian"; "French"; "Greek"; "Indian"; "Irish"; "Italian"; "Jamaican"; "Japanese"; "Kenyan"; "Malaysian"; "Mexican"; "Moroccan"; "Polish"; "Portuguese"; "Russian"; "Spanish"; "Thai"; "Tunisian"; "Tunisian"; "Vietnamese"] in 
   print_list areas

