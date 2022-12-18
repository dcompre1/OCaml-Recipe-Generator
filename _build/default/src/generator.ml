(*implementation of functions in generator.mli*)
open Core
open Lwt
open Cohttp_lwt_unix
module M = Meal

type meal = M.meal

(*
let unimplemented () =
	failwith "unimplemented" *)

let print_list (l : string list) : unit =
  (List.iter ~f:(fun x -> Stdio.printf "%s\n\n" x) l [@coverage off])

let body (uri : string) =
  Client.get (Uri.of_string uri) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  String.drop_suffix (String.drop_prefix body 11) 3
  |> String.filter ~f:(fun c -> Char.( <> ) '\"' c)

let format_body (s : string) : string list =
  String.substr_replace_all ~pattern:", " ~with_:"|" s
  |> String.split ~on:','
  |> List.map ~f:(fun s -> String.substr_replace_all ~pattern:"|" ~with_:", " s)

let tuple_list (l : string list) : (string * string) list =
  List.map
    ~f:(fun s ->
      match String.lsplit2 ~on:':' s with Some x -> x | None -> ("", ""))
    l

(*TODO: make more efficient, right now im making all meals and then comparing ingredients but should do below
  returns list of meal strings that just have id, thumbnail, name
     from here:
     one by one, lookup meal ingredients by ID provided
     check ingredients with restrictions
     if restrictions NOT there, create the meal and store it in a list*)
let format_body2 (s : string) : string list =
  String.substr_replace_all ~pattern:"},{" ~with_:"|" s |> String.split ~on:'|'

let big_body (uri : string) =
  Client.get (Uri.of_string uri) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  String.drop_suffix (String.drop_prefix body 11) 3
  |> String.filter ~f:(fun c -> Char.( <> ) '\"' c)

let find_ingredients (i : string) (ingredients : string list) : bool =
  match ingredients with
  | [] -> false
  | l ->
      List.exists
        ~f:(fun s ->
          String.is_substring ~substring:(String.lowercase i)
            (String.lowercase s))
        l

let format_ingredient (ingredient : string) : string =
  String.substr_replace_all ~pattern:" " ~with_:"_" ingredient
  |> String.lowercase

let format_restrictions (restrictions : string list) : string list =
  List.map ~f:(fun s -> String.lowercase s) restrictions

let has_restrictions (ingredients : string list) (restrictions : string list) :
    bool =
  List.exists
    ~f:(fun i -> Bool.( = ) (find_ingredients i ingredients) true)
    restrictions

let filter_meals (restrictions : string list) (m : meal list) : meal list =
  match restrictions with
  | [] -> m
  | _ ->
      List.filter
        ~f:(fun x ->
          not
            (has_restrictions x.ingredients (format_restrictions restrictions)))
        m

(* figure out if the ID provided only contains digits return false or true
   if false, another function will have to handle this input issue/display an error to user *)
let valid_id (id : string) : bool =
  String.for_all ~f:(fun c -> Char.is_digit c) id && String.length id = 5

(* format a string of a meal name to work with url
   - separate the string by whitespace
   - capitalize each word
   - put the string backtogether with underscores instead of spaces
   return this newly formatted string *)
let format_meal_name (name : string) : string =
  Str.global_replace (Str.regexp "[ \t]+") "_" name

(*
 let print_list_meals (l: meal list): unit = 
   List.iter ~f:M.print_meal l *)

(*
let print_tuples(l: (string * string) list): unit = 
   List.iter ~f:(fun (x, y)-> print_endline x; print_endline y) l
let print_list2 (l: (string * string) list list): unit = 
   List.iter ~f:(fun x -> print_tuples x) l *)
   module type Randomness = sig
      (*
        Given a maximum integer value, return a pseudorandom integer from 0 (inclusive) to this value (exclusive).
      *)
      val int : int -> int
    end
    
let get_meal (module R : Randomness)(m : meal list) : meal =
  match m with
  | [] -> M.empty_meal
  | [ m ] -> m
  | m ->
      let n = R.int (List.length m - 1) in
      List.nth_exn m n

let rec rem_meal (m : meal) (l : meal list) : meal list =
  match l with
  | [] -> []
  | x :: xs -> if M.equal x m then xs else x :: rem_meal m xs

let is_file (filename : string) : string =
  if String.is_suffix ~suffix:".txt" filename then filename
  else filename ^ ".txt"

let no_meals (m : meal list) : bool =
  match m with [] -> true | [ x ] -> M.is_empty x | _ -> false

(*find a meal in a list of meals by its number*)
let find_meal (num : int) (l : meal list) : meal =
  match List.nth l num with None -> M.empty_meal | Some x -> x

let number_list (m : meal list) : string list =
  List.mapi ~f:(fun i x -> (string_of_int i ^ ": "^ x.name)) m


[@@@coverage off]
let get_meal_by_id2 (id : string) : meal =
  if valid_id id then
    let body =
      Lwt_main.run
        (body ("https://www.themealdb.com/api/json/v1/1/lookup.php?i=" ^ id))
    in
    let meal =
      match format_body body |> tuple_list |> M.create_meal with
      | None -> M.empty_meal
      | Some m -> m
    in
    meal
  else M.empty_meal

let find_meals (uri : string) : meal list =
  let body = Lwt_main.run (big_body uri) in
  format_body2 body
  |> List.map ~f:(fun x -> tuple_list (format_body x))
  |> List.map ~f:(fun x -> M.lookup "idMeal" x)
  |> (List.map ~f:(fun x -> get_meal_by_id2 x))

let get_random_meal (filename : string) : unit =
  let body =
    Lwt_main.run (body "https://www.themealdb.com/api/json/v1/1/random.php")
  in
  let meal =
    match format_body body |> tuple_list |> M.create_meal with
    | None -> M.empty_meal
    | Some m -> m
  in
  M.print_meal meal;
  M.meal_to_file meal filename
 


let get_meal_by_name (name : string) (filename : string) : unit =
  let name = format_meal_name name in
  let body =
    Lwt_main.run
      (body ("https://www.themealdb.com/api/json/v1/1/search.php?s=" ^ name))
  in
  let meal =
    match format_body body |> tuple_list |> M.create_meal with
    | None -> M.empty_meal
    | Some m -> m
  in
  if M.is_empty meal then print_endline "No meal exists with this name"
  else M.print_meal meal;
  M.meal_to_file meal filename
  

let get_meal_by_id (filename : string) (id : int) : unit =
  let id = string_of_int id in
  if valid_id id then (
    let body =
      Lwt_main.run
        (body ("https://www.themealdb.com/api/json/v1/1/lookup.php?i=" ^ id))
    in
    let meal =
      match format_body body |> tuple_list |> M.create_meal with
      | None -> M.empty_meal
      | Some m -> m
    in
    if M.is_empty meal then print_endline "No meal exists with this ID"
    else M.print_meal meal;
    M.meal_to_file meal filename)
  else
    print_endline "Invalid ID was given"
   
  
 (*TODO: check for valid filename*)
let rec prompt_for_recipe (m : meal) (l : meal list) (filename : string) : unit
    =
  match l with
  | [] ->
      print_endline
        "Sorry, there are no more recipes that meet the criteria you entered."
  | l -> (
      (M.print_meal m;
       let input =
         printf
           " \n\
            Would you like to receive a different recipe? Please indicate yes \
            or no: %!";
         match In_channel.input_line In_channel.stdin with
         | None -> failwith "no value entered. aborting."
         | Some line -> line
       in
       match input with
       | "yes" | "YES" | "y" ->
           let new_list = rem_meal m l in
           prompt_for_recipe (get_meal (module Random) new_list) new_list filename
       | "no" | "NO" | "n" ->
           let filename =
             match filename with
             | "" -> format_ingredient m.name ^ ".txt"
             | f -> is_file f
           in
           print_endline ("\nYour recipe is now saved in the file:" ^ filename);
           M.meal_to_file m filename
       | _ -> failwith "invalid input")
      
      )

(*TODO: do not execute anything if input is empty or filename is empty, or just have default filename and tell them that
         TODO: make the default filename cute like the name of the recipe or something like that *)
let get_recipe (c : char) (filename : string) (input : string)
    (restrictions : string list) : unit =
  if String.is_empty input then print_endline "No input given"
  else
    let meals =
      match c with
      | 'i' ->
          find_meals
            ("https://www.themealdb.com/api/json/v1/1/filter.php?i="
           ^ format_ingredient input)
      | 'v' ->
          find_meals
            "https://www.themealdb.com/api/json/v1/1/filter.php?c=Vegan"
      | 'g' ->
          find_meals
            "https://www.themealdb.com/api/json/v1/1/filter.php?c=Vegetarian"
      | 'c' ->
          find_meals
            ("https://www.themealdb.com/api/json/v1/1/filter.php?a="
           ^ format_ingredient input)
      | _ -> failwith "don't get here"
    in
    let meals = filter_meals restrictions meals in
    if no_meals meals then
      print_endline "Sorry, there are no recipes that fit your criteria"
    else
      let meal = get_meal (module Random) meals in
      (prompt_for_recipe meal meals filename )

let print_meal_list (m : string list) : unit =
  (List.iter
     ~f:(fun x->
       print_endline x)
     m
  )



   (*TODO: this could probably be combined with prompt_for_recipe by sending prompt strings as parameters *)
   (*TODO: give option 'q' to just terminate program and not save a recipe in file *)
let rec prompt_for_recipe2 (l : meal list) (filename : string) : unit =
  match l with
  | [] ->
      print_endline
        "Sorry, there are no recipes that meet the criteria you entered."
  | l -> (
      (print_endline
         "Here is a list of recipes that meet the criteria you entered:";
       number_list l |> print_meal_list;
       let input1 =
         printf
           "Would you like to view any of these recipes? Please type the \
            number next to the recipe you would like to view, or simply type \
            \"no\": %!";
         match In_channel.input_line In_channel.stdin with
         | None -> failwith "no value entered. aborting."
         | Some line -> line
       in
       match input1 with
       | "no" | "NO" | "n" -> print_endline "Okay, thank you"
       | i -> (
           let i = int_of_string i in
           let meal = find_meal i l in
           M.print_meal meal;
           let filename =
             match filename with
             | "" -> format_ingredient meal.name ^ ".txt"
             | f -> is_file f
           in
           let input2 =
             printf
               "Would you like to view the list of recipes again or save this \
                recipe in %s? \n\
                Type (v) to view recipes again or any press any key to save \
                this recipe %!"
               filename;
             match In_channel.input_line In_channel.stdin with
             | None -> failwith "no value entered. aborting."
             | Some line -> line
           in
           match input2 with
           | "v" | "V" -> prompt_for_recipe2 l filename
           | _ ->
               let filename =
                 match filename with
                 | "" -> format_ingredient meal.name ^ ".txt"
                 | f -> is_file f
               in
               print_endline
                 ("Your recipe has been saved in the file" ^ filename);
               M.meal_to_file meal filename))
      )


let get_recipe_list (c : char) (filename : string) (input : string)
    (restrictions : string list) : unit =
  let meals =
    match c with
    | 'i' ->
        find_meals
          ("https://www.themealdb.com/api/json/v1/1/filter.php?i="
         ^ format_ingredient input)
    | 'v' ->
        find_meals "https://www.themealdb.com/api/json/v1/1/filter.php?c=Vegan"
    | 'g' ->
        find_meals
          "https://www.themealdb.com/api/json/v1/1/filter.php?c=Vegetarian"
    | 'c' ->
        find_meals
          ("https://www.themealdb.com/api/json/v1/1/filter.php?a="
         ^ format_ingredient input)
    | _ -> failwith "don't get here"
  in
  let meals = filter_meals restrictions meals in
  (prompt_for_recipe2 meals filename [@coverage off])

let print_cuisines () : unit =
  print_endline
    "Here is a list of all possible cuisine types available to choose from: ";
  let areas =
    [
      "American";
      "British";
      "Canadian";
      "Chinese";
      "Croatian";
      "Dutch";
      "Egyptian";
      "French";
      "Greek";
      "Indian";
      "Irish";
      "Italian";
      "Jamaican";
      "Japanese";
      "Kenyan";
      "Malaysian";
      "Mexican";
      "Moroccan";
      "Polish";
      "Portuguese";
      "Russian";
      "Spanish";
      "Thai";
      "Tunisian";
      "Tunisian";
      "Vietnamese";
    ]
  in
  print_list areas
