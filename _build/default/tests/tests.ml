(*tests made here*)

(*open Core;; *)
open OUnit2;;
module G = Generator
(*module M = Meal
type meal = M.meal*)

(*let m = {meal.name = ""; meal.id = ""; meal.id = ""; area: string; img : string; vid : string ; ingredients : string list; measurements: string list}; *)

let test_format_ingredient _ = 
  assert_equal "peanut_butter" (G.format_ingredient "Peanut Butter");
  assert_equal "chicken_breast" (G.format_ingredient "Chicken Breast")

let test_has_restrictions _ = 
  assert_equal true (G.has_restrictions ["chicken breast"; "peanut butter"; "soy sauce"] ["chicken"; "peanut"; "soy"]);
  assert_equal true (G.has_restrictions ["chicken"; "peanut"; "soy"] ["chicken"; "peanut"; "soy"]);
  assert_equal false (G.has_restrictions ["sesame seeds"; "butter"; "oil"] ["chicken"; "peanut"; "soy"]);
  assert_equal false (G.has_restrictions ["sesame seeds"; "butter"; "oil"] []);
  assert_equal false (G.has_restrictions [] []);
  assert_equal false (G.has_restrictions [] ["chicken"; "peanut"; "soy"])


let test_find_ingredients _ = 
  assert_equal true (G.find_ingredients "chicken" ["chicken"; "peanut"; "soy"]);
  assert_equal false (G.find_ingredients "butter" ["chicken"; "peanut"; "soy"]);
  assert_equal false (G.find_ingredients "" []);
  assert_equal false (G.find_ingredients "" ["chicken"; "peanut"; "soy"]);
  assert_equal false (G.find_ingredients "chicken" [])

(*use quickcheck for this and for format_meal_name and other format functions*)
let test_valid_id _ =
  assert_equal true (G.valid_id "23456");
  assert_equal false (G.valid_id "adlfj");
  assert_equal false (G.valid_id "123")

let test_format_meal_name _ = 
  assert_equal "Arabiatta_Penne" (G.format_meal_name "arabiatta penne");
  assert_equal "Arabiatta_Penne" (G.format_meal_name "araBIAtta    PEnnE");
  assert_equal "" (G.format_meal_name "")

let project_tests = 
    "Project" 
    >: test_list 
    [
      "Format Ingredient" >:: test_format_ingredient;
      "Valid ID" >:: test_valid_id;
      "Format Meal Name" >:: test_format_meal_name;
      "Find Ingredients" >:: test_find_ingredients;
      "Has Restrictions" >:: test_has_restrictions
    ]
    
  let series =
      "Assignment3 Tests"
      >::: [
             project_tests;
           ]
  
  let () = run_test_tt_main series
    

