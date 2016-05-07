module Foods where

import List exposing (map, foldl)
import Dict exposing (Dict)
import Maybe exposing (Maybe)

type alias FoodDB = Dict Int Food

type Nutrition = Calories Float
               | Recipe (List FoodRef)

type alias Food =
  { id : Int
  , name : String
  , amount : Measurement
  , nutrition : Nutrition
  }

type alias FoodRef =
  { id : Int
  , amount : Measurement
  }

type alias EatenFood =
  { id : Int
  , food : FoodRef
  }

type Measurement
  = Pounds Quantity
  | Ounces Quantity
  | Grams Quantity
  | Milligrams Quantity
  | Container Quantity

type alias Quantity = Float

ratio : Measurement -> Measurement -> Float
ratio mA mB =
  normalized mB / normalized mA

normalized : Measurement -> Float
normalized m =
  case m of
    Ounces a -> 1 * a
    Pounds a -> 0.0625 * a
    Grams a -> 28.3495 * a
    Milligrams a -> 28349.5 * a
    Container a -> 1 * a

lookup : FoodDB -> FoodRef -> Maybe Food
lookup db ref = Dict.get (ref.id) db

sum : List number -> number
sum = foldl (+) 0

foodCalories : FoodDB -> Food -> Float
foodCalories db food =
  case food.nutrition of
    Calories calories -> calories
    Recipe foodRefs ->
      let ingredientCalories ref =
            lookup db ref
              |> Maybe.map (foodCalories db)
              |> Maybe.withDefault 0
      in sum (map ingredientCalories foodRefs)

calories : FoodDB -> EatenFood -> Float
calories db eaten =
  let maybeFood = lookup db eaten.food
      calcCalories food =
        let proportion = ratio food.amount eaten.food.amount
            caloriesForFood = foodCalories db food
        in proportion * caloriesForFood
  in Maybe.map calcCalories maybeFood
     |> Maybe.withDefault 0
