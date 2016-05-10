module Main where

import Html exposing (div, span, Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
  httpTask "/foods" parseFoodDB foodDBStream

port foodLogRequests : Task x ()
port foodLogRequests =
  httpTask "/foodLog" (Json.map .foods parseFoodLog) foodLogStream

httpTask : String -> Json.Decoder a -> Signal.Mailbox a -> Task x ()
httpTask endpoint decoder mailbox =
  (Http.get decoder ("http://localhost:3000" ++ endpoint)
  |> (mapError parseHttpError))
  `andThen` (Signal.send mailbox.address)
  `onError` (Signal.send actionMailbox.address << ShowError)


actionMailbox : Signal.Mailbox Action
actionMailbox = Signal.mailbox Noop

foodDBStream : Signal.Mailbox FoodDB
foodDBStream = Signal.mailbox Dict.empty

foodLogStream : Signal.Mailbox (List EatenFood)
foodLogStream = Signal.mailbox []

foodSummaryStream : Signal.Signal (List FoodSummary)
foodSummaryStream =
  Signal.map2 toSummary foodDBStream.signal foodLogStream.signal

toSummary : FoodDB -> List EatenFood -> List FoodSummary
toSummary db foods =
  let summarize (eaten, food) =
        { id = eaten.id
        , name = food.name
        , calories = calories db eaten
        }
  in List.map summarize (inflate db foods)

type alias FoodSummary =
  { id : Int
  , name : String
  , calories : Float
  }

type Action = UpdateDB FoodDB
            | UpdateFoodLog FoodLog
            | ShowError String
            | AddFoodDialog
            | Noop

type alias Model =
  { error : String
  , db : FoodDB
  , maybeFoodLog : Maybe FoodLog
  , addFoodDialog : Bool
  }

emptyModel : Model
emptyModel = { error = ""
             , db = Dict.empty
             , maybeFoodLog = Nothing
             , addFoodDialog = False
             }

main = Signal.map2 view foodSummaryStream (Signal.foldp update emptyModel actionMailbox.signal)

update : Action -> Model -> Model
update action model =
  case action of
    Noop -> model
    AddFoodDialog -> { model | addFoodDialog = True }
    ShowError e -> { model | error = e }
    UpdateDB newDB -> { model | db = newDB }
    UpdateFoodLog newLog -> { model | maybeFoodLog = Just newLog }

view : List FoodSummary -> Model -> Html
view summaries model = if model.error /= ""
                       then Html.text model.error
                       else render (dashboard summaries model)

dashboard : List FoodSummary -> Model -> Node
dashboard summaries {addFoodDialog} =
  let totalCalories =
        List.foldl (+) 0 (List.map .calories summaries)
      dialog = nodes [ text "Add text dialog" ]
  in nodes [ text (toString totalCalories)
           , node "button" [text "Add food"]
               |> withAttr (onClick actionMailbox.address AddFoodDialog)
           , if addFoodDialog
             then dialog
             else text ""
           , foodsList2 summaries ]

calcTotalCalories : FoodDB -> List EatenFood -> Float
calcTotalCalories db foods =
  List.foldl (+) 0 (List.map (calories db) foods)

foodsList2 : List FoodSummary -> Node
foodsList2 summaries =
  let toItem summary =
        { icon = Chicken
        , mainText = summary.name
        , subText = toString summary.calories ++ " calories"
        , minorText = ""
        }
  in card [ list (List.map toItem summaries) ]

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
