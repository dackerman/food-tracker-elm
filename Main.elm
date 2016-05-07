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
import Foods exposing (..)

type Action = Not

foodResults : Signal.Mailbox (Result String (Dict Int Food))
foodResults = Signal.mailbox (Err "")

port foodRequests : Task x ()
port foodRequests =
  (Http.get parseFoodDB "http://localhost:3000/foods"
  |> (mapError (\e -> case e of
                        Http.UnexpectedPayload m -> m
                        a -> toString a))
  |> toResult)
  `andThen` Signal.send foodResults.address

main2 = Signal.map (\v -> Html.pre [] [Html.text (case v of
                                                  Err msg -> msg
                                                  a -> toString a)]) foodResults.signal

type alias Model =
  { db : FoodDB
  , today : List EatenFood
  }

emptyModel = { db = Dict.empty, today = [] }

main = render (foodsList emptyModel)

foodsList : Model -> Node
foodsList {db, today} =
  let columns = [ { name = "Name", fn = (\(eaten, food) -> food.name) }
                , { name = "Amount", fn = (\(eaten, food) -> toString food.amount) }
                , { name = "Calories", fn = (\(eaten, food) -> toString <| foodCalories db food) }
                ]
  in card [ grid columns (inflate db today) ]
