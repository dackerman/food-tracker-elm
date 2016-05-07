module Main where

import Html exposing (text, div, span, Attribute, Html)
import Html.Attributes exposing (style)
import List exposing (concat, map, append)
import Maybe exposing (Maybe(..), withDefault)

import Model exposing (..)
import Styles exposing (..)
import Grid exposing (..)
import Card exposing (..)

groundBeef : Food
groundBeef = Ingredient
             { name = "Ground Beef"
             , calories = 332.1
             , amount = Ounces 4
             }

chickenBreast : Food
chickenBreast = Ingredient
                { name = "Chicken Breast"
                , calories = 1
                , amount = Grams 50
                }

chipotleChicken : Food
chipotleChicken = Ingredient
                  { name = "Chipotle Chicken"
                  , calories = 160
                  , amount = Grams 100
                  }

chipotleFajitaVegetables : Food
chipotleFajitaVegetables = Ingredient
                           { name = "Chipotle Fajita Vegetables"
                           , calories = 20
                           , amount = Grams 30
                           }

chipotleBlackBeans : Food
chipotleBlackBeans = Ingredient
                     { name = "Chipotle Black Beans"
                     , calories = 70
                     , amount = Grams 80
                     }

burritoBowl : Food
burritoBowl = Food
              { name = "Burrito Bowl"
              , amount = Ounces 8
              , items =
                [ chipotleChicken
                , chipotleFajitaVegetables
                , chipotleBlackBeans
                ]
              }


model : Model
model =
  { foods = [ { food = groundBeef
              , amount = Ounces 2
              }
            , { food = chickenBreast
              , amount = Grams 100
              }
            , { food = burritoBowl
              , amount = Ounces 8
              }
            ]
  , currentMonth = "March"
  }

type Action = Not

update : Action -> Model -> Model
update a m = m

main = render <| foodsList model

foodsList : Model -> Node
foodsList {foods} =
  let columns = [ { name = "Name", fn = (\{food} -> name food) }
                , { name = "Amount", fn = (\{food} -> toString <| amount food) }
                , { name = "Calories", fn = (\instance -> toString <| calories instance) }
                ]
  in card [ grid columns foods ]
