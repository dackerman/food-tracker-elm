module Model where

import List exposing (map, foldl)

type alias Model =
  { foods : List FoodInstance
  , currentMonth : String
  }


type alias FoodInstance =
  { food : Food
  , amount : Measurement
  }

name : Food -> String
name f = case f of
           Food {name} -> name
           Ingredient {name} -> name


amount : Food -> Measurement
amount f = case f of
           Food {amount} -> amount
           Ingredient {amount} -> amount


type Food
  = Food
    { name : String
    , items : List Food
    , amount : Measurement
    }
  | Ingredient
    { name : String
    , calories : Float
    , amount : Measurement
    }


type alias Quantity = Float


type Measurement
  = Pounds Quantity
  | Ounces Quantity
  | Grams Quantity
  | Milligrams Quantity
  | Container Quantity

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

type Macro = Macro Protein Fat Carb

type alias Protein = Float
type alias Fat = Float
type alias Carb = Float

foodCalories : Food -> Float
foodCalories food =
  case food of
    Ingredient {calories} ->
      calories

    Food {items} ->
      foldl (+) 0 <| map foodCalories items


calories : FoodInstance -> Float
calories foodInstance =
  let food = foodInstance.food
      amount = case food of
                 Food {amount} -> amount
                 Ingredient {amount} -> amount
      proportion = ratio amount foodInstance.amount
      caloriesForFood = foodCalories food
  in proportion * caloriesForFood
