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

type alias FoodLog =
  { id : Int
  , user : Int
  , year : Int
  , month : Int
  , day : Int
  , foods : List EatenFood
  }

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

isPresent : Maybe a -> Bool
isPresent m =
  case m of
    Just _ -> True
    Nothing -> False

filterMaybes : List (Maybe a) -> List a
filterMaybes maybes =
  let pickJusts m xs = case m of
                         Just x -> x :: xs
                         Nothing -> xs
  in List.foldl pickJusts [] maybes

inflate : FoodDB -> List EatenFood -> List (EatenFood, Food)
inflate db eatenFoods =
  map (\eaten -> Maybe.map ((,) eaten) (lookup db eaten.food)) eatenFoods
    |> filterMaybes

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
