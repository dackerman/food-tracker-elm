module FoodJson
  ( jsonToFoods
  ) where

import Json.Decode as Json exposing
  ( (:=)
  , int
  , float
  , string
  , object4
  , object2
  , oneOf
  , tuple2
  , customDecoder)

import Dict exposing (Dict)

import Foods exposing (..)

jsonToFoods : Json.Decoder (Dict Int Food)
jsonToFoods =
  Json.list (oneOf [parseIngredient, parseRecipe])
    |> Json.map Dict.fromList

parseIngredient : Json.Decoder (Int, Food)
parseIngredient =
  let food = object4
             Ingredient
             ("id" := int)
             ("name" := string)
             parseAmount
             ("calories" := float)
  in food
    |> (Json.map I)
    |> (Json.map indexed)

parseRecipe : Json.Decoder (Int, Food)
parseRecipe =
  let food = object4
             Recipe
               ("id" := int)
               ("name" := string)
               parseAmount
               ("foods" := (Json.list parseIngredientRef))
  in food
    |> (Json.map C)
    |> (Json.map indexed)

indexed : Food -> (Int, Food)
indexed food =
  case food of
    C contents -> (contents.id, food)
    I contents -> (contents.id, food)

parseIngredientRef : Json.Decoder IngredientRef
parseIngredientRef =
  object2
    IngredientRef
    ("id" := int)
    parseAmount

parseAmount : Json.Decoder Measurement
parseAmount =
  let tryParseAmount (amount, units) =
        case units of
          "pounds" -> Ok <| Pounds amount
          "ounces" -> Ok <| Ounces amount
          "grams" -> Ok <| Grams amount
          "milligrams" -> Ok <| Milligrams amount
          "container" -> Ok <| Container amount
          _ -> Result.Err ("Invalid units: " ++ units)
      parseTuple = object2 (,) ("amount" := float) ("units" := string)
  in customDecoder parseTuple tryParseAmount
