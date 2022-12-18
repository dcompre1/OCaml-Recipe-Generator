open OUnit2;;
module G = Generator
module M = Meal

let meal1_info = [("idMeal", "52771");("strMeal", "Arrabiata Penne"); ("strInstructions", "Instructions.info");
("strArea", "Italian"); ("strMealThumb", "Image_URL"); ("strYoutube", "Video_URL"); ("strIngredient1", "peanut");
("strIngredient2", "chicken"); ("strMeasure1", "2"); ("strMeasure2", "3")]

let meal2_info =  [("idMeal", "52492");("strMeal", "Omelette"); ("strInstructions", "Instructions.info");
("strArea", "American"); ("strMealThumb", "Image_URL"); ("strYoutube", "Video_URL"); ("strIngredient1", "egg");
("strIngredient2", "cheese"); ("strMeasure1", "2"); ("strMeasure2", "3")]

let meal3_info =  [("idMeal", "50032");("strMeal", "Pancakes"); ("strInstructions", "Instructions.info");
("strArea", "American"); ("strMealThumb", "Image_URL"); ("strYoutube", "Video_URL"); ("strIngredient1", "flour");
("strIngredient2", "sugar"); ("strMeasure1", "2"); ("strMeasure2", "3")]


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
  assert_equal "Italian" (M.lookup "strArea" meal1_info);
  assert_equal "" (M.lookup "strArea" []);
  assert_equal "52771" (M.lookup "idMeal" meal1_info);
  assert_equal "" (M.lookup "Ingredient" meal1_info)

let test_get_ingredients _ = 
  assert_equal ["peanut"; "chicken"] (M.get_ingredients meal1_info);
  assert_equal [] (M.get_ingredients [])

let test_get_measurements _ = 
  assert_equal ["2"; "3"] (M.get_measurements meal1_info);
  assert_equal [] (M.get_measurements [])

let test_get_meal_info _ = 
  assert_equal (Some ("Arrabiata Penne", "52771", "Instructions.info", "Italian", "Image_URL", "Video_URL")) (M.get_meal_info meal1_info);
  assert_equal None (M.get_meal_info [])

let test_merge_with _ = 
  assert_equal ["1. 1/4 cup Paprika"; "2. 1 Egg"] (M.merge_with ["Paprika"; "Egg"] ["1/4 cup"; "1"] 1);
  assert_equal ["Paprika"; "Egg"] (M.merge_with ["Paprika"; "Egg"] [] 1);
  assert_equal [] (M.merge_with [] [] 1);
  assert_equal [] (M.merge_with [] ["1"; "2"] 1);
  assert_equal ["1. Egg"; "2. Butter"] (M.merge_with ["Egg"; "Butter"] [""; ""] 1)

let meal1 = match M.create_meal meal1_info with |None -> M.empty_meal |Some m -> m
let meal2 = match M.create_meal meal2_info with |None -> M.empty_meal |Some m -> m
let meal3 = match M.create_meal meal3_info with |None -> M.empty_meal |Some m -> m

let meal_list = [meal1; meal2; meal3]

let test_ordered _ = 
  assert_equal ["1. Instructions"; "2. info"] (M.get_ordered_instructions meal1);
  assert_equal [] (M.get_ordered_instructions M.empty_meal)

  
  let test_rem_meal _ =  
    assert_equal [meal1; meal2] (G.rem_meal meal3 meal_list);
    assert_equal [meal1; meal2; meal3] (G.rem_meal M.empty_meal meal_list);
    assert_equal [] (G.rem_meal M.empty_meal [])
  
  let test_is_file _ = 
    assert_equal "filename.txt" (G.is_file "filename");
    assert_equal "file.txt" (G.is_file "file.txt")

  let test_no_meals _ = 
    assert_equal true (G.no_meals []);
    assert_equal false (G.no_meals meal_list);
    assert_equal true (G.no_meals [M.empty_meal])

  let test_find_meal _ = 
    assert_equal meal2 (G.find_meal 1 meal_list);
    assert_equal M.empty_meal (G.find_meal 1 []);
    assert_equal M.empty_meal (G.find_meal 4 meal_list)

  
  let test_number_list _ = 
    assert_equal ["0: Arrabiata Penne"; "1: Omelette"; "2: Pancakes"] (G.number_list meal_list);
    assert_equal [] (G.number_list [])


    module My_Rand : G.Randomness = struct
      let int (n : int) : int = n
    end

let test_get_meal _ = 
  assert_equal meal3 (G.get_meal (module My_Rand) meal_list);
  assert_equal M.empty_meal (G.get_meal (module My_Rand) []);
  assert_equal meal1 (G.get_meal (module My_Rand) [meal1])
  


(*TODO: make two parts: generator tests and meal tests*)
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
      "Ordered" >:: test_ordered;
      "Remove Meal" >:: test_rem_meal;
      "Is File" >:: test_is_file;
      "No Meals" >:: test_no_meals;
      "Find Meal" >:: test_find_meal;
      "Number List" >:: test_number_list;
      "Get Meal" >:: test_get_meal
    ]
    
  let series =
      "Assignment3 Tests"
      >::: [
             project_tests;
           ]
  
  let () = run_test_tt_main series
