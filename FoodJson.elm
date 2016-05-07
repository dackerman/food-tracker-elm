module FoodJson
  ( parseFoodDB
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

parseFoodDB : Json.Decoder FoodDB
parseFoodDB =
  Json.list parseFood
    |> Json.map Dict.fromList

parseFood : Json.Decoder (Int, Food)
parseFood =
  let food =
        object4
          Food
          ("id" := int)
          ("name" := string)
          parseAmount
          ("nutrition" := parseNutrition)
  in food
    |> Json.map indexed

parseNutrition : Json.Decoder Nutrition
parseNutrition =
  oneOf [ Json.map Calories float
        , Json.map Recipe (Json.list parseFoodRef) ]

parseFoodRef : Json.Decoder FoodRef
parseFoodRef =
  object2
    FoodRef
    ("id" := int)
    parseAmount

indexed : Food -> (Int, Food)
indexed food = (food.id, food)

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
