module Main where

import Html exposing (text, div, span, Attribute, Html)
import Html.Attributes exposing (style)
import List exposing (concat, map, append)
import Dict exposing (Dict)
import Maybe exposing (Maybe(..), withDefault)
import Task exposing (..)
import Json.Decode as Json

import Http

import Styles exposing (..)
import Grid exposing (..)
import Card exposing (..)
import FoodJson exposing (..)
import Foods exposing (..)

parseHttpError : Http.Error -> String
parseHttpError error =
  case error of
    Http.Timeout ->
      "timeout"

    Http.NetworkError ->
      "network error"

    Http.UnexpectedPayload msg ->
      msg

    Http.BadResponse code msg ->
      toString code ++ ": " ++ msg

port foodRequests : Task x ()
port foodRequests =
  httpTask "/foods" parseFoodDB UpdateDB
{-
port foodLogRequests : Task x ()
port foodLogRequests =
  httpTask "/foodLog" parseFoodLog UpdateFoodLog
-}
httpTask : String -> Json.Decoder a -> (a -> Action) -> Task x ()
httpTask endpoint decoder onSuccess =
  (Http.get decoder ("http://localhost:3000" ++ endpoint)
  |> (mapError parseHttpError))
  `andThen` (Signal.send actionMailbox.address << onSuccess)
  `onError` (Signal.send actionMailbox.address << ShowError)


actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox Noop

type Action = UpdateDB FoodDB
            | ShowError String
            | Noop

type alias Model =
  { error : String
  , db : FoodDB
  , foodLog : List EatenFood
  }

testEatenFood : EatenFood
testEatenFood =
  { id = 8
  , food =
    { id = 1
    , amount = Ounces 3
    }
  }

emptyModel : Model
emptyModel = { error = ""
             , db = Dict.empty
             , foodLog = [testEatenFood] }

main = Signal.map view (Signal.foldp update emptyModel actionMailbox.signal)

update : Action -> Model -> Model
update action model =
  case action of
    Noop -> model
    ShowError e -> { model | error = e }
    UpdateDB newDB -> { model | db = newDB }

view : Model -> Html
view model = if model.error /= ""
             then Html.text model.error
             else render (foodsList model)

foodsList : Model -> Node
foodsList {db, foodLog} =
  let columns = [ { name = "Name", fn = (\(eaten, food) -> food.name) }
                , { name = "Amount", fn = (\(eaten, food) -> toString eaten.food.amount) }
                , { name = "Calories", fn = (\(eaten, food) -> toString <| calories db eaten) }
                ]
  in card [ grid columns (inflate db foodLog) ]
