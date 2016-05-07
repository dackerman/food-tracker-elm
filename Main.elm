module Main where

import Html exposing (text, div, span, Attribute, Html)
import Html.Attributes exposing (style)
import List exposing (concat, map, append)
import Dict exposing (Dict)
import Maybe exposing (Maybe(..), withDefault)
import Task exposing (..)

import Http

import Styles exposing (..)
import Grid exposing (..)
import Card exposing (..)
import FoodJson exposing (..)

type Action = Not

foodResults : Signal.Mailbox (Result String (Dict Int Food))
foodResults = Signal.mailbox (Err "")

port foodRequests : Task x ()
port foodRequests =
  (Http.get jsonToFoods "http://localhost:3000/foods"
  |> (mapError (\e -> case e of
                        Http.UnexpectedPayload m -> m
                        a -> toString a))
  |> toResult)
  `andThen` Signal.send foodResults.address

main = Signal.map (\v -> Html.pre [] [Html.text (case v of
                                                  Err msg -> msg
                                                  a -> toString a)]) foodResults.signal

main2 = render <| foodsList {foods = []}

foodsList : {foods : List FoodInstance} -> Node
foodsList {foods} =
  let columns = [ { name = "Name", fn = (\{food} -> food.name) }
                , { name = "Amount", fn = (\{food} -> toString food.amount) }
                , { name = "Calories", fn = (\instance -> toString <| calories instance) }
                ]
  in card [ grid columns foods ]
