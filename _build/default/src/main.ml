open Core
module G = Generator
[@@@ocaml.warning "-32"]

let rand = 
  Command.basic 
  ~summary: "get a random recipe"
  Command.Let_syntax.(
    let%map_open filename = anon (maybe_with_default "my_recipe.txt"("filename" %: string))  in fun () -> G.get_random_meal filename)

let prompt_for_value name of_string = 
  printf "Please Enter the %s of the recipe you would like to find: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

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
    fun () -> G.get_meal_by_id id filename)

let print_list (l : string list) : unit =
  List.iter ~f:(fun x -> Stdio.printf "%s " x) l

let prompt_for_name name = 
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line 

let name = 
  Command.basic 
  ~summary: "get a recipe based on the meal name"
  ( 
    let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
    in let meal_name = prompt_for_name "Meal Name" in
    let filename = 
      match filename with  
      | Some x -> x
      | None -> prompt_for_filename "filename"

    in fun () -> G.get_meal_by_name meal_name filename) 

let prompt_for_ingredient ingredient = 
  printf "enter %s: %!" ingredient;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line

let ingredient = 
  Command.basic
  ~summary: "get a random recipe that contains user-specified ingredient"
  ( 
    let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
    and  restrictions = anon (maybe_with_default [] (non_empty_sequence_as_list ("restrictions" %: string)))
    in let ingredient = prompt_for_ingredient "ingredient" in
    let filename = 
      match filename with  
      | Some x -> x
      | None -> prompt_for_filename "filename"
  in fun () -> print_endline ingredient; print_endline filename; print_list restrictions) 

let cuisine = 
  Command.basic 
  ~summary: "get recipe based on a cuisine type and list of ingredients to exclude"
  (
    let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
    and see_cuisines = flag "-s" no_arg ~doc:"see list of possible cuisines to choose from"
    and cuisine = flag "-c" (optional string) ~doc:"cuisine"
    and restrictions = anon (maybe_with_default [] (non_empty_sequence_as_list ("restrictions" %: string)))
  in 
  if see_cuisines then G.print_cuisines else
  let filename, cuisine = 
  match filename, cuisine with
  | Some x, Some y -> x, y
  | Some x, None -> x, prompt_for_value "Cuisine Type" String.of_string
  | None, Some y -> prompt_for_filename "filename", y
  | None, None -> prompt_for_filename "filename",  prompt_for_value "Cuisine Type" String.of_string
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

(*
let prompt_for_command: string = 
  printf "\nTo begin the program, view the choices below: \n
  \"rand \": Receive a completely random recipe
  \"id\": lookup a recipe from the ID of the meal
  \"name\": lookup a recipe from the name of the meal
  \"ingredient\": Receive a random recipe that includes a specified main ingredient
  \"cuisine\": Receive a random recipe from a specified region
  \"vegan\": Receive a vegan recipe
  \"vegetarian\": Receive a vegetarian recipe\n

  Please indicate your choice below, Thank you!
  %!";
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line *)

let format_command command =
  String.filter ~f:(fun c ->  Char.is_alpha c) command 

let command = 
  Command.group
  ~summary: "receive recipes"
  ["rand", rand; "id", id; "name", name; "ingredient", ingredient; "cuisine", cuisine; "vegan", vegan; "vegetarian", vegetarian]

let () = Command_unix.run command
(*let p = prompt_for_command in 
match format_command p with 
| "" -> print_endline "try again"
| "rand" -> Command_unix.run rand
| "id" -> Command_unix.run id
| "name" -> Command_unix.run name
| "ingredient" -> Command_unix.run ingredient 
| "cuisine" -> Command_unix.run cuisine 
| "vegan" -> Command_unix.run vegan 
| "vegetarian" -> Command_unix.run vegetarian 
| p -> printf "\n\"%s\" is not a valid command, try again\n" p*)
