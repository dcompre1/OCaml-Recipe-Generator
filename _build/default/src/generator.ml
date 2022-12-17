(*implementation of functions in generator.mli*)
open Core
open Lwt
open Cohttp_lwt_unix

module M = Meal
type meal = M.meal

[@@@ocaml.warning "-27"]

let unimplemented () =
	failwith "unimplemented"

let print_list (l : string list) : unit =
   List.iter ~f:(fun x -> Stdio.printf "%s\n\n" x) l

let body (uri: string)= 
   Client.get (Uri.of_string uri) >>= fun (_, body) ->
   body |> Cohttp_lwt.Body.to_string >|= fun body -> String.drop_suffix (String.drop_prefix body 11) 3|> String.filter ~f:(fun c -> Char.(<>) '\"' c)

let format_body (s: string): string list = 
   String.substr_replace_all ~pattern:", " ~with_:"|" s|> String.split ~on:',' |> List.map ~f:( fun s -> String.substr_replace_all ~pattern:"|" ~with_:", " s)

let tuple_list (l: string list): (string * string) list = 
   List.map ~f:(fun s -> match String.lsplit2 ~on:':' s with |Some x -> x |None -> "","") l

(*returns list of meal strings that just have id, thumbnail, name
   from here:
   one by one, lookup meal ingredients by ID provided
   check ingredients with restrictions 
   if restrictions NOT there, create the meal and store it in a list*)
let format_body2 (s: string): string list = 
   String.substr_replace_all ~pattern:"},{" ~with_:"|" s|> String.split ~on:'|' 

(*
let print_meal_list (l: (string * string list) list): unit =
   List.iter ~f:(fun (x, y) -> print_endline (x ^ "\n"); print_list y; print_string "\n\n\n") l  *)

let big_body(uri: string)=
   Client.get (Uri.of_string uri) >>= fun (_,body) ->
   body |> Cohttp_lwt.Body.to_string >|= fun body -> String.drop_suffix (String.drop_prefix body 11) 3|> String.filter ~f:(fun c -> Char.(<>) '\"' c)


(* sanitize input string and return a list of possible ingredients to avoid that include this string

   - all non space/non alpha characters dropped
   - capitalize each word
   
   examples: 
      "cheese" would return ["Cheddar Cheese"; "Swiss Cheese"; ...]
      "Chicken" would return ["Chicken"; "Chicken Breast"; "Chicken Stock"; ...]  

   possibly include an algorithm that can take care of misspelled words, this likely won't be implemented unless I find I have a lot of extra time *)

(*replace with List.for_all syntax*)
let find_ingredients (i: string)(ingredients: string list): bool =
      match ingredients with 
      | [] -> false
      | l -> List.exists ~f:(fun s -> String.is_substring ~substring:(String.lowercase i)  (String.lowercase s)) l

      (*
      let find_ingredients2 (i: string)(ingredients: string list): string list=
         match ingredients with 
         | [] -> []
         | l -> List.map ~f:(fun s -> string_of_bool (String.is_substring ~substring:(String.lowercase i) (String.lowercase s))) l *)
   
   
(* ingredients need to be in a specific format to use with url
   - Spaces changed into underscores
   - all other whitespace/non alpha characters dropped
   - all lowercase

   ex: "Peanut Butter" -> "peanut_butter"
*)
let format_ingredient (ingredient: string): string = 
   String.substr_replace_all ~pattern:" " ~with_:"_" ingredient |> String.lowercase 

(* for all of the strings in the list, make a call to format_ingredient and replace/fix each string 
   return the corrected list of strings
   separate string by whitespace and then call format_ingredient on each value in this list*)
let format_restrictions(restrictions: string list): string list = 
   List.map ~f:(fun s -> String.lowercase s) restrictions

(* simple helper method that will compare two lists and return true if they have any elements in common *)
let has_restrictions (ingredients: string list) (restrictions: string list): bool = 
   (*match ingredients with 
   |[] -> false
   | l -> match restrictions with 
         | [] -> false 
         | x :: xs -> if find_ingredients x l then true else has_restrictions l xs 
   *)(*List.map ~f:(fun x -> find_ingredients x ingredients) (format_restrictions restrictions) |> List.exists ~f:(fun b -> Bool.(=) true b) *)
   List.exists ~f:(fun i -> Bool.(=) (find_ingredients i ingredients) true ) restrictions

(* take in a list of meals
   get_ingredients of each meal and call has_restrictions on a provided list of restrictions, do not return any meals that contain user's restricted ingredients *)
let filter_meals (restrictions: string list) (m: meal list): meal list = 
  (* match m with 
   | [] -> []
   | x :: xs -> if has_restrictions (x.ingredients) restrictions then x :: filter_meals restrictions xs else filter_meals restrictions xs *)
   match restrictions with 
   | [] -> m
   | _ -> List.filter ~f:(fun x -> not (has_restrictions x.ingredients (format_restrictions restrictions))) m
 
(*
let print_has (ingredients: string list)(restrictions: string list): unit = 
   List.iter ~f:(fun r -> print_endline (string_of_bool (find_ingredients r ingredients))) restrictions

let f (restrictions: string list) (m: meal list): unit = 
      (* match m with 
       | [] -> []
       | x :: xs -> if has_restrictions (x.ingredients) restrictions then x :: filter_meals restrictions xs else filter_meals restrictions xs *)
       (*List.iter ~f:(fun x -> print_list x.ingredients; print_endline (string_of_bool (has_restrictions x.ingredients restrictions))) m *)
       List.iter ~f:(fun x -> print_has x.ingredients restrictions) m
       (*match restrictions with 
       | [] -> []
       | _ -> List.map ~f:(fun x -> string_of_bool (not (has_restrictions x.ingredients (format_restrictions restrictions)))) m *)
*)     

(*make an API request to: www.themealdb.com/api/json/v1/1/random.php, save information as meal *)
(*make same as rand_meal above*)

let get_random_meal (filename: string): unit = 
   let body = Lwt_main.run (body "https://www.themealdb.com/api/json/v1/1/random.php") in
   let meal = (match format_body body |> tuple_list |> M.create_meal with |None -> M.empty_meal(*("","","","","","",[],[])*)|Some m -> m) in 
   M.print_meal meal;
   M.meal_to_file meal filename

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
   Str.global_replace (Str.regexp "[ \t]+") "_" name

(* make call to format_meal_name
   use url for finding a meal by its name:  www.themealdb.com/api/json/v1/1/search.php?s=Arrabiata *)
let get_meal_by_name (name: string) (filename: string): unit = 
   let name = format_meal_name name in
   let body = Lwt_main.run (body ("https://www.themealdb.com/api/json/v1/1/search.php?s=" ^ name)) in
   let meal = (match format_body body |> tuple_list |> M.create_meal with |None -> M.empty_meal|Some m -> m) in 
   M.print_meal meal;
   M.meal_to_file meal filename

 let get_meal_by_id2 (filename: string)(id: string): meal = 
      if valid_id id then 
      let body = Lwt_main.run (body ("https://www.themealdb.com/api/json/v1/1/lookup.php?i=" ^ id)) in
      let meal = (match format_body body |> tuple_list |> M.create_meal with |None -> M.empty_meal|Some m -> m) in meal
      else M.empty_meal

(* string is the ID value
   call valid_id
   use url to find a meal by its ID:  www.themealdb.com/api/json/v1/1/lookup.php?i=52772 and return the meal*)
let get_meal_by_id (filename: string)(id: int): unit = 
   let id = string_of_int id in
   if valid_id id then 
   let body = Lwt_main.run (body ("https://www.themealdb.com/api/json/v1/1/lookup.php?i=" ^ id)) in
   let meal = (match format_body body |> tuple_list |> M.create_meal with |None -> M.empty_meal|Some m -> m) in 
   M.print_meal meal;
   M.meal_to_file meal filename;
   else print_endline "Invalid ID was given"

   (*change to return meal list option*)

(*
 let print_list_meals (l: meal list): unit = 
   List.iter ~f:M.print_meal l *)

(*
let print_tuples(l: (string * string) list): unit = 
   List.iter ~f:(fun (x, y)-> print_endline x; print_endline y) l
let print_list2 (l: (string * string) list list): unit = 
   List.iter ~f:(fun x -> print_tuples x) l *)
   
let find_meals (c: char) (s: string): meal list =

   let body = Lwt_main.run (big_body ("https://www.themealdb.com/api/json/v1/1/filter.php?i=" ^ s)) in
   format_body2 body |> List.map ~f:(fun x -> tuple_list (format_body x))|> List.map ~f:(fun x -> M.lookup "idMeal" x) |> List.map ~f:(fun x -> get_meal_by_id2 "" x) 
   (*make call to API, get a series of lists into one big list, call create_meal on each meal in that list*)

(* return a randomly selected meal from a list of meals *)
let get_meal (m: meal list): meal = 
   match m with 
   | [] -> M.empty_meal
   | m -> let n = Random.int ((List.length m) - 1) in List.nth_exn m n

   let get_ingredient_list (filename: string)(ingredient: string)(restrictions: string list): unit = 
      let meal = 
      find_meals 'c' (format_ingredient ingredient)|> filter_meals restrictions |> get_meal in  M.print_meal meal; M.meal_to_file meal filename
   


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

let print_cuisines (): unit = 
   print_endline "Here is a list of all possible cuisine types available to choose from: ";
   let areas = ["American"; "British"; "Canadian"; "Chinese"; "Croatian"; "Dutch"; "Egyptian"; 
   "French"; "Greek"; "Indian"; "Irish"; "Italian"; "Jamaican"; "Japanese"; "Kenyan"; "Malaysian";
   "Mexican"; "Moroccan"; "Polish"; "Portuguese"; "Russian"; "Spanish"; "Thai"; "Tunisian";
   "Tunisian"; "Vietnamese"] 
   in print_list areas

