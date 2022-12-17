
(*open OUnit2;;
module G = Generator
module M = Meal

let meal_info = [("idMeal", "52771");("strMeal", "Arrabiata Penne"); ("strInstructions", "Instructions info");
("strArea", "Italian"); ("strMealThumb", "Image_URL"); ("strYoutube", "Video_URL"); ("strIngredient1", "peanut");
("strIngredient2", "chicken"); ("strMeasure1", "2"); ("strMeasure2", "3")]

let meal = "", "", "Do This.Do That", "", "", "", [], []

let test_format_ingredient _ = 
  assert_equal "peanut_butter" (G.format_ingredient "Peanut Butter");
  assert_equal "chicken_breast" (G.format_ingredient "Chicken Breast")

let test_format_restrictions _ = 
  assert_equal ["chicken"; "peanut"] (G.format_restrictions ["CHICKEN"; "PEANUT"]);
  assert_equal [] (G.format_restrictions [])
  
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
  assert_equal false (G.find_ingredients "chicken" [])

let test_valid_id _ =
  assert_equal true (G.valid_id "23456");
  assert_equal false (G.valid_id "adlfj");
  assert_equal false (G.valid_id "123")

let test_format_meal_name _ = 
  assert_equal "arabiatta_penne" (G.format_meal_name "arabiatta penne");
  assert_equal "araBIAtta_PEnnE" (G.format_meal_name "araBIAtta    PEnnE");
  assert_equal "" (G.format_meal_name "")

let test_lookup _ = 
  assert_equal "Italian" (G.lookup "strArea" meal_info);
  assert_equal "" (G.lookup "strArea" []);
  assert_equal "52771" (G.lookup "idMeal" meal_info);
  assert_equal "" (G.lookup "Ingredient" meal_info)

let test_get_ingredients _ = 
  assert_equal ["peanut"; "chicken"] (G.get_ingredients meal_info);
  assert_equal [] (G.get_ingredients [])

let test_get_measurements _ = 
  assert_equal ["2"; "3"] (G.get_measurements meal_info);
  assert_equal [] (G.get_measurements [])

let test_get_meal_info _ = 
  assert_equal (Some ("52771", "Arrabiata Penne", "Instructions info", "Italian", "Image_URL", "Video_URL")) (G.get_meal_info meal_info);
  assert_equal None (G.get_meal_info [])

let test_merge_with _ = 
  assert_equal ["1. 1/4 cup Paprika"; "2. 1 Egg"] (M.merge_with ["Paprika"; "Egg"] ["1/4 cup"; "1"] 1);
  assert_equal ["Paprika"; "Egg"] (M.merge_with ["Paprika"; "Egg"] [] 1);
  assert_equal [] (M.merge_with [] [] 1)

let test_ordered _ = 
  assert_equal ["1. Do This"; "2. Do That"] (M.get_ordered_instructions meal)

let project_tests = 
    "Project" 
    >: test_list 
    [
      "Format Ingredient" >:: test_format_ingredient;
      "Valid ID" >:: test_valid_id;
      "Format Meal Name" >:: test_format_meal_name;
      "Find Ingredients" >:: test_find_ingredients;
      "Has Restrictions" >:: test_has_restrictions;
      "Format Restrictions" >:: test_format_restrictions;
      "Lookup" >:: test_lookup;
      "Get Ingredients" >:: test_get_ingredients;
      "Get Measurements" >:: test_get_measurements;
      "Get Meal Info" >:: test_get_meal_info;
      "Merge With" >:: test_merge_with;
      "Ordered" >:: test_ordered
    ]
    
  let series =
      "Assignment3 Tests"
      >::: [
             project_tests;
           ]
  
  let () = run_test_tt_main series
    
*)
