(*implementation of functions in meal.mli*)
open Core

(*name * id * instructions * area * img * vid * ingredients * measurements*)
type meal =  string * string * string * string * string * string * string list * string list
(*{name : string; id : string; instructions : string; area: string; img : string; vid : string ; ingredients : string list; measurements: string list} [@@deriving yojson] *)

[@@@ocaml.warning "-27"]


let rec merge_with (l: string list) (l2: string list) (n: int): string list = 
  match l, l2 with 
  | [], [] -> []
  | [], y -> failwith "shouldn't get here"
  | x, [] -> x
  | x :: xs, y :: ys -> ( string_of_int n ^ ". " ^ y ^ " of " ^ x) :: merge_with xs ys (n + 1)

(*with a meal as input , return the list of its ingredients
   example: "chicken", "cheese", "onion"*)
let get_ingredients(m: meal)(n: int): string list = 
   let (_,_,_,_,_,_,ingredients,measurements) = m in
  match ingredients with 
  | [] -> []
  | x -> merge_with x measurements 1

(*helper function to get ingredients so they are ordered: ["1..."; "2..."; ...]*)
let add_order (l: string list): string list = 
  List.mapi ~f:(fun i s -> (string_of_int (i + 1)) ^ ". " ^ s) l  

(*with a meal as input, return the string that indicates the area the meal originates from
   examples: "Indian", "Chinese", "Mexican"*)
let get_instructions (m: meal): string list = 
   let (_,_,instructions,_,_,_,_,_) = m in
   match instructions with 
   | "" -> []
   | x -> add_order (String.split_on_chars ~on:['.'] x |> List.filter ~f:(fun s -> String.(=) s ""))

(* return a string option of video url from meal type 
   return None if there is no video associated with the meal, return Some URL_STRING if a video exists*)   
let get_video (m: meal): string option =
   let (_,_,_,_,vid,_,_,_) = m in
   match vid with 
   | "" -> None
   | s -> Some s

(* return a string option of image url from meal type 
   return None if there is no image associated with the meal, return Some URL_STRING if an image exists*)   
let get_img (m: meal): string option = 
   let (_,_,_,_,_,img,_,_) = m in
   match img with 
   | "" -> None
   | s -> Some s

let print_list (l : string list) : unit =
   List.iter ~f:(fun x -> Stdio.printf "%s\n\n " x) l

(*this function will only be used if a frontend is not created for this project, print meal info to file*)
let print_meal (m: meal) (filename: string): unit =  
   let (name,id,_,area,_,_,_,_) = m in
   print_endline name;
   print_endline id;
   print_endline area;
   print_list (get_ingredients m 1);
   print_list (get_instructions m);
   let vid = 
      match (get_video m) with 
      |None -> "Youtube video not available"
      |Some v -> v
   in print_endline vid;
   let img = 
      match (get_img m) with 
      |None -> "Image not available"
      |Some i -> i
   in print_endline img;