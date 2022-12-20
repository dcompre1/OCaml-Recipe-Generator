(*implementation of functions in generator.mli*)
open Core
open Lwt
open Cohttp_lwt_unix
module M = Meal

type meal = M.meal

(*body of smaller API request -- exactly one recipe *)
let body (uri : string) =
  Client.get (Uri.of_string uri) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  String.drop_suffix (String.drop_prefix body 11) 3
  |> String.filter ~f:(fun c -> Char.( <> ) '\"' c)

(*get list of each component of the recipe*)
let format_body (s : string) : string list =
  String.substr_replace_all ~pattern:", " ~with_:"|" s
  |> String.split ~on:','
  |> List.map ~f:(fun s -> String.substr_replace_all ~pattern:"|" ~with_:", " s)

(*reformat list of components as string * string list to aid in searching process later
   ex: [("strMeal", meal_name); ("strID", meal_ID) ... ]*)
let tuple_list (l : string list) : (string * string) list =
  List.map
    ~f:(fun s ->
      match String.lsplit2 ~on:':' s with Some x -> x | None -> ("", ""))
    l

(*body of larger API request -- >= one recipe*)
let big_body (uri : string) =
  Client.get (Uri.of_string uri) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  String.drop_suffix (String.drop_prefix body 11) 3
  |> String.filter ~f:(fun c -> Char.( <> ) '\"' c)

(*get list of recipe strings to call format_body on*)
let format_big_body (s : string) : string list =
  String.substr_replace_all ~pattern:"},{" ~with_:"|" s |> String.split ~on:'|'

(*from a  list of ingredients, return boolean if any contain string i:
   example:
   "cheese" ["Cheddar Cheese"; "Bread"] -> true
   "cheese" ["peanut"; "chicken" ] -> false*)
let find_ingredients (i : string) (ingredients : string list) : bool =
  match ingredients with
  | [] -> false
  | l ->
      List.exists
        ~f:(fun s ->
          String.is_substring ~substring:(String.lowercase i)
            (String.lowercase s))
        l

(*ex: "Peanut Butter" -> "peanut_butter"*)
let format_ingredient (ingredient : string) : string =
  String.substr_replace_all ~pattern:" " ~with_:"_" ingredient
  |> String.lowercase

(*make list of strings all lowercase*)
let format_restrictions (restrictions : string list) : string list =
  List.map ~f:(fun s -> String.lowercase s) restrictions

(*compare list of ingredients with list of restrictions and return true if any of the restrictions is in the list of ingredients*)
let has_restrictions (ingredients : string list) (restrictions : string list) :
    bool =
  List.exists
    ~f:(fun i -> Bool.( = ) (find_ingredients i ingredients) true)
    restrictions

(*filter out any meals from a meal list that have restrictions*)
let filter_meals (restrictions : string list) (m : meal list) : meal list =
  match restrictions with
  | [] -> m
  | _ ->
      List.filter
        ~f:(fun x ->
          not
            (has_restrictions x.ingredients (format_restrictions restrictions)))
        m

(*return true if id is all digits*)
let valid_id (id : string) : bool =
  String.for_all ~f:(fun c -> Char.is_digit c) id && String.length id = 5

let format_meal_name (name : string) : string =
  Str.global_replace (Str.regexp "[ \t]+") "_" name

(*helps with testing get_meal*)
module type Randomness = sig
  val int : int -> int
end

(*returns a random meal from a list of meals*)
let get_meal (module R : Randomness) (m : meal list) : meal =
  match m with
  | [] -> M.empty_meal
  | [ m ] -> m
  | m ->
      let n = R.int (List.length m - 1) in
      List.nth_exn m n

(*removes meal m from list l*)
let rec rem_meal (m : meal) (l : meal list) : meal list =
  match l with
  | [] -> []
  | x :: xs -> if M.equal x m then xs else x :: rem_meal m xs

(*format filename to be a valid .txt name or return it if it's already correct form*)
let format_file (filename : string) (name : string) : string =
  match filename with
  | "" ->
      format_meal_name
        (String.filter
           ~f:(fun c -> Char.is_alpha c or Char.is_whitespace c)
           name)
      ^ ".txt"
  | f -> if String.is_suffix ~suffix:".txt" f then f else f ^ ".txt"

(*check if a meal list is an empty list or a list with just one empty meal*)
let no_meals (m : meal list) : bool =
  match m with [] -> true | [ x ] -> M.is_empty x | _ -> false

(*find a meal in a list of meals from its index*)
let find_meal (num : int) (l : meal list) : meal =
  match List.nth l num with None -> M.empty_meal | Some x -> x

(*returns numbered list of meal names*)
let number_list (m : meal list) : string list =
  List.mapi ~f:(fun i x -> string_of_int i ^ ": " ^ x.name) m

[@@@coverage off]

(*return the meal from associated id string*)
let get_meal_by_id (id : string) : meal =
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

(*make API call, format it, create meals and store them in a list*)
let find_meals (uri : string) : meal list =
  let body = Lwt_main.run (big_body uri) in
  format_big_body body
  |> List.map ~f:(fun x -> tuple_list (format_body x))
  |> List.map ~f:(fun x -> M.lookup "idMeal" x)
  |> List.map ~f:(fun x -> get_meal_by_id x)

(*make API request to get meal and print out one random meal + store in file*)
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
  M.meal_to_file meal (format_file filename meal.name)

(*make API request to get meal and print it out + store in file*)
let print_meal_by_name (name : string) (filename : string) : unit =
  if String.is_empty name then print_endline "No name was entered"
  else
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
    M.meal_to_file meal (format_file filename meal.name)

(*make API request to get a meal and print it out + store in file*)
let print_meal_by_id (filename : string) (id : string) : unit =
  if String.is_empty id then print_endline "no ID was entered"
  else if valid_id id then (
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
    M.meal_to_file meal (format_file filename meal.name))
  else print_endline "Invalid ID was given"

(*continuous prompting while meals available in meal list*)
let rec prompt_for_recipe (m : meal) (l : meal list) (filename : string) : unit
    =
  match l with
  | [] ->
      print_endline
        "Sorry, there are no more recipes that meet the criteria you entered."
  | l -> (
      M.print_meal m;
      let input =
        printf
          " \n\
           Would you like to receive a different recipe? Please indicate yes \
           to recieve a new recipe or no to save this recipe in a file, or \
           enter any key to quit the program : %!";
        match In_channel.input_line In_channel.stdin with
        | None -> failwith "no value entered. aborting."
        | Some line -> line
      in
      match input with
      | "yes" | "YES" ->
          let new_list = rem_meal m l in
          prompt_for_recipe
            (get_meal (module Random) new_list)
            new_list filename
      | "no" | "NO" ->
          let filename = format_file filename m.name in
          print_endline ("\nYour recipe is now saved in the file: " ^ filename);
          M.meal_to_file m filename
      | _ -> print_endline "Thank you!")

(*printing recipe based on 4 diff paths:
   main ingredient, vegan, vegetarian, cuisine*)
let print_recipe (c : char) (filename : string) (input : string)
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
      prompt_for_recipe meal meals filename

(*print out all meal options*)
let print_meal_list (m : string list) : unit =
  List.iter ~f:(fun x -> print_endline x) m

let rec prompt_for_recipe_list (l : meal list) (filename : string) : unit =
  if no_meals l then
    print_endline "Sorry, there are no meals that fit your criteria"
  else (
    print_endline
      "Here is a list of recipes that meet the criteria you entered:";
    number_list l |> print_meal_list;
    let input1 =
      printf
        "\n\
         Would you like to view any of these recipes? Please enter the number \
         next to the recipe you would like to view, or simply enter \"no\": %!";
      match In_channel.input_line In_channel.stdin with
      | None -> failwith "no value entered. aborting."
      | Some line -> line
    in
    match input1 with
    | "no" | "NO" | "n" -> print_endline "Thank you!"
    | i -> (
        if String.is_empty i then print_endline "No input"
        else if not (String.for_all ~f:(fun c -> Char.is_digit c) i) then
          print_endline "Invalid input"
        else
          let i = int_of_string i in
          let meal = find_meal i l in
          M.print_meal meal;
          let filename = format_file filename meal.name in
          let input2 =
            printf
              "\n\
               Would you like to view the list of recipes again or save this \
               recipe in %s? \n\
               Enter (v) to view recipes again or (s) to save this recipe, \
               otherwise enter any key to quit %!"
              filename;
            match In_channel.input_line In_channel.stdin with
            | None -> failwith "no value entered. aborting."
            | Some line -> line
          in
          match input2 with
          | "v" | "V" -> prompt_for_recipe_list l filename
          | "s" | "S" ->
              let filename = format_file filename meal.name in
              print_endline
                ("Your recipe has been saved in the file: " ^ filename);
              M.meal_to_file meal filename
          | _ -> print_endline "Thank you!"))

let print_recipe_list (c : char) (filename : string) (input : string)
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
  (prompt_for_recipe_list meals filename [@coverage off])

let print_cuisines () : unit =
  print_endline
    "Here is a list of all possible cuisines available to choose from: ";
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
  M.print_list areas
