(*implementation of functions in meal.mli*)
open Core

(*name * id * instructions * area * img * vid * ingredients * measurements*)
type meal =  {name: string; id: string; instructions: string; area: string;  img:string;  vid: string; ingredients:string list;  measurements:string list}

let empty_meal = {name = ""; id = ""; instructions = ""; area = ""; img = ""; vid = ""; ingredients = []; measurements = []}
(*string * string * string * string * string * string * string list * string list*)

(*
let get_name(m: meal): string = (*let {name;_;_;_;_;_;_;_} = m in name*)

let get_id(m: meal): string = let {_;id;_;_;_;_;_;_} = m in id
let get_instructions(m: meal): string = let (_,_,instructions,_,_,_,_,_) = m in instructions
let get_area(m: meal): string = let (_,_,_,area,_,_,_,_) = m in area
let get_vid(m: meal): string = let (_,_,_,_,_,vid,_,_) = m in vid
let get_ingredients(m: meal): string list = let (_,_,_,_,_,_,ingredients,_) = m in ingredients 
let get_measurements(m: meal): string list =  let (_,_,_,_,_,_,_,measurements) = m in measurements *)

let rec merge_with (l: string list) (l2: string list) (n: int): string list = 
  match l, l2 with 
  | [], [] -> []
  | [], _ -> failwith "shouldn't get here"
  | x, [] -> x
  | x :: xs, y :: ys -> match x,y with 
                        | "", "" -> []
                        | x, "" -> ( string_of_int n ^ ". " ^ x) :: merge_with xs ys (n + 1)
                        | "", _ -> []
                        | x, y -> ( string_of_int n ^ ". " ^ y ^ " " ^ x) :: merge_with xs ys (n + 1)

let get_ordered_ingredients(m: meal): string list = 
  match m.ingredients with 
  | [] -> []
  | x -> merge_with x m.measurements 1


let add_order (l: string list): string list = 
  List.mapi ~f:(fun i s -> (string_of_int (i + 1)) ^ ". " ^ s) l  


let get_ordered_instructions (m: meal): string list = 
   match m.instructions with 
   | "" -> []
   | x -> add_order (String.split_on_chars ~on:['.'] x |> List.filter ~f:(fun s -> String.(<>) s ""))

(* return a string option of video url from meal type 
   return None if there is no video associated with the meal, return Some URL_STRING if a video exists*)   
let get_video (m: meal): string option =
   match m.vid with 
   | "" -> None
   | s -> Some s

(* return a string option of image url from meal type 
   return None if there is no image associated with the meal, return Some URL_STRING if an image exists*)   
let get_img (m: meal): string option = 
   match m.img with 
   | "" -> None
   | s -> Some s


let rec get_ingredients (l: (string * string) list): string list = 
      match l with 
      | [] -> []
      | (key, value) :: xs -> if String.is_prefix ~prefix:"strIngred" key then value :: get_ingredients xs else get_ingredients xs
   
let rec get_measurements (l: (string * string) list): string list = 
      match l with 
      | [] -> []
      | (key, value) :: xs -> if String.is_prefix ~prefix:"strMeasure" key then value :: get_measurements xs else get_measurements xs

      
let rec lookup (k : string) (m : (string * string) list): string  =
   match m with
   | [] -> ""
   | (k2, v) :: vs -> if String.compare k k2 <> 0 then lookup k vs else v

let get_meal_info (l: (string * string) list): (string * string * string * string * string * string) option= 
   match l with 
   | [] -> None
   | l -> Some ((lookup "strMeal" l), (lookup "idMeal" l), (lookup "strInstructions" l), (lookup "strArea" l), (lookup "strMealThumb" l), (lookup "strYoutube" l))

let create_meal (l: (string * string) list): meal option= 
   match get_meal_info l with 
   | None -> None
   | Some (n, i, ins, a, im, v) -> Some {name = n; id = i; instructions = ins; area = a; img = im; vid = v; ingredients = get_ingredients l; measurements = get_measurements l}

let print_list (l : string list) : unit =
   List.iter ~f:(fun x -> Stdio.printf "%s\n " x) l

[@@@coverage off]
let print_meal (m: meal): unit =  
   Stdio.printf "Meal Name: %s\n" m.name;
   Stdio.printf "Meal ID: %s\n" m.id;
   Stdio.printf "Area: %s\n" m.area;
   print_endline "Ingredients: ";
   print_list (get_ordered_ingredients m);
   print_endline "";
   print_endline "Instructions";
   print_list (get_ordered_instructions m);
   let vid = 
      match (get_video m) with 
      |None -> "Youtube video not available"
      |Some v -> v
   in Stdio.printf "Youtube video: %s\n" vid;
   let img = 
      match (get_img m) with 
      |None -> "Image not available"
      |Some i -> i
   in Stdio.printf "Image: %s\n" img

let meal_to_file (m: meal)(filename: string): unit = 
   Arg.write_arg filename (
   Array.concat [
      [| 
      "Meal Name: " ^ m.name; 
      "Meal ID: " ^ m.id;
      "Area: \n" ^ m.area;
      "Ingredients: "; 
      |]; 
      (List.to_array (get_ordered_ingredients m));
      [|"\nInstructions: "|];
      (List.to_array (get_ordered_instructions m));
      [|(let vid = 
         match (get_video m) with 
         |None -> "Youtube video not available"
         |Some v -> v
      in "Youtube video: " ^ vid);
      (let img = 
         match (get_img m) with 
         |None -> "Image not available"
         |Some i -> i
      in "Image: " ^ img)|]
      ]
      );
   