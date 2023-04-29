Project: Recipe Generator
------------------------------------

Dinamary Compres

### API URLs
TheMealDB: https://www.themealdb.com/api.php

random meal: www.themealdb.com/api/json/v1/1/random.php

all meal categories: www.themealdb.com/api/json/v1/1/list.php?c=list
all areas: www.themealdb.com/api/json/v1/1/list.php?a=list
all ingredients: www.themealdb.com/api/json/v1/1/list.php?i=list

find by main ingredient: www.themealdb.com/api/json/v1/1/filter.php?i=[ingredient lowercase and underscores no spaces]
find by main category: www.themealdb.com/api/json/v1/1/filter.php?c=[cat. Capitalized]
find by area: www.themealdb.com/api/json/v1/1/filter.php?a=[area Capitalized]

lookup meal by id: www.themealdb.com/api/json/v1/1/lookup.php?i=52772

lookup meal by name: www.themealdb.com/api/json/v1/1/search.php?s=Arrabiata

### Structure
The recipe generator will take user input from the command line. 
Recipes will be acquired from TheMealDB public API. It will access the
API’s list of recipes so a user can choose ones that exclude restrictions they have.

User can indicate their dietary restrictions (vegetarian/vegan) and any
ingredients they want to exclude from potential recipes. 

From there, makes an API call to TheMealDB, create a dictionary/list of possible recipes that meets
the user’s specifications and then they will receive a recipe from that collection. 

The program should output message to indicate when no recipes are available that work for user's choices. 

Creates a file with the full information of the meal: the meal name, id, cuisine type, instructions, ingredients, youtube link, image

This app is inspired by an app I created over the summer in python but with completely different user interaction, linked here: https://github.com/dcompre1/SEO_Week_2

Possible Libraries:
- Core
- Cohttp lwt unix
- Dream
- ReScript

### Order of Implementation
- make command paths work just to begin structure of program 
    - implement all the commands and flags indicated in main.ml
    - for this step I will likely just make them all print something to the terminal to verify they're working

- --cuisines flag: prints out full list of cuisines from TheMealDB DONE
    - get data from TheMealDB into a list and print it out to stdout
    - in generator.ml implement print_cuisines

- rand command: get 1 recipe randomly selected from all of the TheMealDB recipes 
    - make API request to TheMealDB url for random recipe 
    - extract all meal information from this and print it to a file 
    - in generator.ml implement get_random_meal, print_meal

- id command: lookup 1 recipe from TheMealDB from an id provided by the user 
    - make API request to TheMealDB url for id lookup
    - print this meal information to a file 
    - in generator.ml implement get_meal_by_id

- name command: lookup 1 recipe from TheMealDB from a meal name provided by the user 
    - make API request to TheMealDB url for name lookup 
    - print this meal information to a file
    - in generator.ml implement get_meal_by_name

- ingredient command: produce 1 random recipe that includes an ingredient indicated by user 
    - make API request to TheMealDB url for ingredient lookup
    - in generator.ml implement get_meal, filter_meals, find_meals, get_ingredients

- vegan command: produce 1 random vegan recipe from a list of recipes gotten from TheMealDB and filtered based on a list of ingredient restrictions from the user
    - make API request to TheMealDB
    - filter meals 
    - in generator.ml implement get_vegan_meal, has_restrictions

- vegetarian command: produce 1 random vegetarian recipe from a list of recipes gotten from TheMealDB and filtered based on a list of ingredient restrictions from the user
    - make API request to TheMealDB
    - filter meals 
    - in generator.ml implement get_vegetarian_recipe

- cuisine command: produce 1 random recipe from a list of recipes of a certain cuisine type gotten from TheMealDB and filtered based on a list of ingredient restrictions from the user
    - make API request
    - filter meals
    - in generator.ml implement get_area
