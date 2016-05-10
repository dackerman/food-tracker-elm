module Main where

import Html exposing (div, span, Attribute, Html)
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
import Components.List exposing (..)

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

port foodLogRequests : Task x ()
port foodLogRequests =
  httpTask "/foodLog" parseFoodLog UpdateFoodLog

httpTask : String -> Json.Decoder a -> (a -> Action) -> Task x ()
httpTask endpoint decoder onSuccess =
  (Http.get decoder ("http://localhost:3000" ++ endpoint)
  |> (mapError parseHttpError))
  `andThen` (Signal.send actionMailbox.address << onSuccess)
  `onError` (Signal.send actionMailbox.address << ShowError)


actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox Noop

type Action = UpdateDB FoodDB
            | UpdateFoodLog FoodLog
            | ShowError String
            | Noop

type alias Model =
  { error : String
  , db : FoodDB
  , maybeFoodLog : Maybe FoodLog
  }

emptyModel : Model
emptyModel = { error = ""
             , db = Dict.empty
             , maybeFoodLog = Nothing }

main = Signal.map view (Signal.foldp update emptyModel actionMailbox.signal)

update : Action -> Model -> Model
update action model =
  case action of
    Noop -> model
    ShowError e -> { model | error = e }
    UpdateDB newDB -> { model | db = newDB }
    UpdateFoodLog newLog -> { model | maybeFoodLog = Just newLog }

view : Model -> Html
view model = if model.error /= ""
             then Html.text model.error
             else render (dashboard model)

dashboard : Model -> Node
dashboard model =
  foodsList model

calcTotalCalories : FoodDB -> List EatenFood -> Float
calcTotalCalories db foods =
  List.foldl (+) 0 (List.map (calories db) foods)

foodsList : Model -> Node
foodsList {db, maybeFoodLog} =
  let foods = Maybe.withDefault [] (Maybe.map .foods maybeFoodLog)
      totalCalories = calcTotalCalories db foods
      toItem (eaten, food) = { icon = if food.name == "Ground Beef"
                                      then Chicken
                                      else Burrito
                             , mainText = food.name
                             , subText = toString (calories db eaten) ++ " calories"
                             , minorText = toString eaten.food.amount }
  in card [ text (toString totalCalories ++ " total calories")
          , list (List.map toItem (inflate db foods)) ]


foodsGrid : Model -> Node
foodsGrid {db, maybeFoodLog} =
  let foods = Maybe.withDefault [] (Maybe.map .foods maybeFoodLog)
      columns = [ { name = "Name", fn = (\(eaten, food) -> food.name) }
                , { name = "Amount", fn = (\(eaten, food) -> toString eaten.food.amount) }
                , { name = "Calories", fn = (\(eaten, food) -> toString <| calories db eaten) }
                ]
  in card [ grid columns (inflate db foods) ]
