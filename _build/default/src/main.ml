open Core
[@@@ocaml.warning "-32"]
(*TODO: CREATE PROMPTS FOR EACH COMMAND THAT WILL ALLOW THEM TO JUST PUT IN NAME LIKE "NAME" AND THEN THE PROGRAM WILL PROMPT FOR INFO AND TELL THEM WHAT KIND OF INFO I WANT*)
(*TODO: POSSIBLE CHANGE THIS FILE TO BE A SERIES OF FLAGS INSTEAD OF GROUP OF COMMANDS*)

(*path to get a random recipe*)
(*figure out how to make basic command that doesn't need an argument, maybe this needs to be written as a series of flags instead of how it is now*)
let rand = 
  Command.basic 
  ~summary: "get a random recipe"
  Command.Let_syntax.(
    let%map_open filename = anon (maybe_with_default "my_recipe.txt"("filename" %: string))  in fun () -> print_endline filename)

let prompt_for_value name of_string = 
  printf "Please Enter the %s of the recipe you would like to find: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

(*path to get a recipe from an inputted ID number *)
let prompt_for_filename filename = 
  printf "enter %s: %!" filename;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line 

let id = 
  Command.basic 
  ~summary: "get a recipe based on an ID number"
  ( 
    let%map_open.Command id = flag "-i" (optional int) ~doc:"id number"
    and filename = anon (maybe_with_default "my_recipe.txt"("filename" %: string))  
  in
  let id =
  match id with 
  | Some x -> x
  | None ->  prompt_for_value "ID" Int.of_string 
  in

    (*id = anon (maybe ("id" %: int)) and filename = anon (maybe ("filename" %: string)) in 
    let id, filename = 
      match id, filename with 
      | Some x, Some y -> x, y
      | None, Some y -> prompt_for_id "id" Int.of_string, y
      | Some x, None -> x, prompt_for_filename "filename"
      | None, None -> prompt_for_id "id" Int.of_string, prompt_for_filename "filename"
    in *)
    fun () -> print_endline (string_of_int id); print_endline filename)

let print_list (l : string list) : unit =
  List.iter ~f:(fun x -> Stdio.printf "%s " x) l

let prompt_for_name name = 
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line 
(*path to get a recipe from an inputted meal name*)
let name = 
  Command.basic 
  ~summary: "get a recipe based on the meal name"
  ( 
    let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
    and meal_name = anon (maybe (non_empty_sequence_as_list ("meal_name" %: string)))
    in 
    let filename, meal_name = 
    match filename, meal_name with 
    | Some x, Some y -> x, y
    | Some x, None -> x, String.split ~on:' ' (prompt_for_value "Meal Name" String.of_string)
    | None, Some y -> prompt_for_filename "filename", y
    | None, None -> prompt_for_filename "filename", String.split ~on:' ' (prompt_for_value "Meal Name" String.of_string)

    (*name = anon (maybe (non_empty_sequence_as_list ("init_words" %: string))) in
    let name = 
      match name with 
      | Some x -> List.to_string ~f:(fun s -> s) x |> String.filter ~f:(fun c -> Char.(<>) ')' c && Char.(<>) '(' c)
      | None -> prompt_for_name "name" *)
    in fun () -> print_list meal_name; print_endline filename) 

let prompt_for_ingredient ingredient = 
  printf "enter %s: %!" ingredient;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line

(*let prompt_for_restrictions restrictions of_string = *)
(*prompt for excluding ingredients after getting the main one we want*)
(* TODO: FIGURE OUT HOW TO ADD RESTRICTIONS TO THIS COMMAND AND ALSO GET THE FULL INGREDIENT NAME, flags might help?*)
let ingredient = 
  Command.basic
  ~summary: "get a random recipe that contains user-specified ingredient"
  ( 
    let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
    and ingredient = (anon (maybe ((non_empty_sequence_as_list ("ingredient" %: string))))) 
    in 
    let filename, ingredient = 
    match filename, ingredient with 
    | Some x, Some y -> x, List.to_string ~f:(fun s -> s) y |> String.filter ~f:(fun c -> Char.(<>) '(' c && Char.(<>) ')' c)
    | Some x, None -> x, (prompt_for_value "Main Ingredient" String.of_string)
    | None, Some y -> prompt_for_filename "filename", List.to_string ~f:(fun s -> s) y|> String.filter ~f:(fun c -> Char.(<>) '(' c && Char.(<>) ')' c)
    | None, None -> prompt_for_filename "filename", (prompt_for_value "Main Ingredient" String.of_string)
  in fun () -> print_endline ingredient; print_endline filename) 
(*path to get a recipe from an inputted cuisine name and optional list of restrictions*)
let cuisine = 
  Command.basic 
  ~summary: "get recipe based on a cuisine type and list of ingredients to exclude"
  (
    let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
    and cuisine = flag "-c" (optional string) ~doc:"cuisine"
    and restrictions = anon (maybe_with_default [] (non_empty_sequence_as_list ("restrictions" %: string)))
  in 
  let filename, cuisine = 
  match filename, cuisine with
  | Some x, Some y -> x, y
  | Some x, None -> x, prompt_for_value "Cuisine Type" String.of_string
  | None, Some y -> prompt_for_filename "filename", y
  | None, None -> prompt_for_filename "filename",  prompt_for_value "Cuisine Type" String.of_string
  (*Command.Let_syntax.( 
    let%map_open cuisine = anon ("cuisine" %: string) and ings = anon (non_empty_sequence_as_list ("init_words" %: string)) *)
  in fun () -> print_endline cuisine; print_list restrictions; print_endline filename)

let vegan = 
  Command.basic 
  ~summary: "get vegan recipe, exclude list of ingredients if provided by user"
  (
   let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename" 
   and restrictions = anon (maybe_with_default [] (non_empty_sequence_as_list ("restrictions" %: string))) 
   in 
   let filename =
    match filename with 
    | Some x -> x
    | None -> "my_recipe.txt" 
   in
   fun () -> print_endline filename; print_list restrictions)

let vegetarian = 
  Command.basic 
  ~summary: "get vegetarian recipe, exclude list of ingredients if provided by user"
    (
  let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename" 
  and restrictions = anon (maybe_with_default [] (non_empty_sequence_as_list ("restrictions" %: string))) 
  in 
  let filename =
   match filename with 
   | Some x -> x
   | None -> "my_recipe.txt" 
  in
  fun () -> print_endline filename; print_list restrictions)

(*group together commands above as well as tags for showing list of cuisine types available and list of ingredients available to users*)
let command = 
  Command.group
  ~summary: "receive recipes"
  ["rand", rand; "id", id; "name", name; "ingredient", ingredient; "cuisine", cuisine; "vegan", vegan; "vegetarian", vegetarian]


(*give info about what they can do, what they want to do, let them choose, then run command based on that*)
let () = Command_unix.run vegetarian