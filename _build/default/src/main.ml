open Core
module G = Generator

[@@@ocaml.warning "-32"]

let rand =
  Command.basic ~summary:"get a random recipe"
    Command.Let_syntax.(
      let%map_open filename =
        anon (maybe_with_default "my_recipe.txt" ("filename" %: string))
      in
      fun () -> G.get_random_meal filename)

let prompt_for_value name of_string =
  printf "Please Enter the %s of the recipe you would like to find: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

let prompt_for_filename filename =
  printf "Enter a filename%s: %!" filename;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line

let id =
  Command.basic ~summary:"get a recipe based on an ID number"
    (let%map_open.Command id = flag "-i" (optional int) ~doc:"id number"
     and filename = flag "-f" (optional string) ~doc:"filename" in
     let id =
       match id with Some x -> x | None -> prompt_for_value "ID" Int.of_string
     in
     let filename =
       match filename with
       | Some x -> x
       | None -> prompt_for_filename "filename"
     in
     fun () -> G.get_meal_by_id filename id)

let print_list (l : string list) : unit =
  List.iter ~f:(fun x -> Stdio.printf "%s " x) l

let prompt_for_name name =
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line

let name =
  Command.basic ~summary:"get a recipe based on the meal name"
    (let%map_open.Command filename =
       flag "-f" (optional string) ~doc:"filename"
     in
     let meal_name = prompt_for_name "Meal Name" in
     let filename =
       match filename with
       | Some x -> x
       | None -> prompt_for_filename "filename"
     in

     fun () -> G.get_meal_by_name meal_name filename)

let prompt_for_ingredient ingredient =
  printf "enter %s: %!" ingredient;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> line

let ingredient =
  Command.basic
    ~summary:"get a random recipe that contains user-specified ingredient"
    (let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
     and list = flag "-l" no_arg ~doc:" list"
     and restrictions =
       anon
         (maybe_with_default []
            (non_empty_sequence_as_list ("restrictions" %: string)))
     in
     let ingredient = prompt_for_ingredient "ingredient" in
     let filename =
       match filename with
       | Some x -> x
       | None -> prompt_for_filename "filename"
     in
     fun () ->
       if list then G.get_recipe_list 'i' filename ingredient restrictions
       else G.get_recipe 'i' filename ingredient restrictions)

let cuisine =
  Command.basic
    ~summary:
      "get recipe based on a cuisine type and list of ingredients to exclude"
    (let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
     and see_cuisines =
       flag "-s" no_arg ~doc:"see list of possible cuisines to choose from"
     and cuisine = flag "-c" (optional string) ~doc:"cuisine"
     and list = flag "-l" no_arg ~doc:" list"
     and restrictions =
       anon
         (maybe_with_default []
            (non_empty_sequence_as_list ("restrictions" %: string)))
     in
     if see_cuisines then G.print_cuisines
     else
       let filename, cuisine =
         match (filename, cuisine) with
         | Some x, Some y -> (x, y)
         | Some x, None -> (x, prompt_for_value "Cuisine Type" String.of_string)
         | None, Some y -> (prompt_for_filename "filename", y)
         | None, None ->
             ( prompt_for_filename "filename",
               prompt_for_value "Cuisine Type" String.of_string )
       in
       fun () ->
         if list then G.get_recipe_list 'c' filename cuisine restrictions
         else G.get_recipe 'c' filename cuisine restrictions)

let vegan =
  Command.basic
    ~summary:"get vegan recipe, exclude list of ingredients if provided by user"
    (let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
     and list = flag "-l" no_arg ~doc:" list"
     and restrictions =
       anon
         (maybe_with_default []
            (non_empty_sequence_as_list ("restrictions" %: string)))
     in
     let filename =
      match filename with
      | Some x -> x
      | None -> prompt_for_filename "filename"
     in
     fun () ->
       if list then G.get_recipe_list 'v' filename "" restrictions
       else G.get_recipe 'v' filename "" restrictions)

let vegetarian =
  Command.basic
    ~summary:
      "get vegetarian recipe, exclude list of ingredients if provided by user"
    (let%map_open.Command filename = flag "-f" (optional string) ~doc:"filename"
     and list = flag "-l" no_arg ~doc:" list"
     and restrictions =
       anon
         (maybe_with_default []
            (non_empty_sequence_as_list ("restrictions" %: string)))
     in
     let filename =
      match filename with
      | Some x -> x
      | None -> prompt_for_filename "filename"
    in
     fun () ->
       if list then G.get_recipe_list 'g' filename "" restrictions
       else G.get_recipe 'g' filename "" restrictions)

let format_command command = String.filter ~f:(fun c -> Char.is_alpha c) command

let command =
  Command.group ~summary:"receive recipes"
    [
      ("rand", rand);
      ("id", id);
      ("name", name);
      ("ingredient", ingredient);
      ("cuisine", cuisine);
      ("vegan", vegan);
      ("vegetarian", vegetarian);
    ]

let () = Command_unix.run command
