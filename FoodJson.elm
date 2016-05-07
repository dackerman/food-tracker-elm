module FoodJson
  ( jsonToFoods
  , FoodJson(..)
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

import Model exposing (..)

type FoodJson = I IngredientJson
              | C CompoundFoodJson

type alias IngredientJson =
  { id : Int
  , name : String
  , amount : Measurement
  , calories : Float
  }

type alias CompoundFoodJson =
  { id : Int
  , name : String
  , amount : Measurement
  , items : List SubIngredientJson
  }

type alias SubIngredientJson =
  { id : Int
  , amount : Measurement
  }

jsonToFoods : Json.Decoder (List FoodJson)
jsonToFoods =
  Json.list (oneOf [parseIngredient, parseCompoundFood])

parseIngredient : Json.Decoder FoodJson
parseIngredient =
  Json.map I <| object4
        IngredientJson
        ("id" := int)
        ("name" := string)
        parseAmount
        ("calories" := float)

parseCompoundFood : Json.Decoder FoodJson
parseCompoundFood =
  Json.map C <| object4
      CompoundFoodJson
        ("id" := int)
        ("name" := string)
        parseAmount
        ("foods" := (Json.list parseSubIngredient))

parseSubIngredient : Json.Decoder SubIngredientJson
parseSubIngredient =
  object2
    SubIngredientJson
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
