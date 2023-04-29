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
