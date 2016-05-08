module Components.List
  ( ListIcon(..)
  , Icon(..)
  , ListItem
  , list
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)

import List exposing (map)

import Debug

type ListIcon = NoListIcon
              | Avatar

type Icon = NoIcon
          | Star

type alias ListItem =
  { icon : ListIcon
  , title : String
  , body : String
  , secondaryIcon : Icon
  }

list : List ListItem -> Html
list items =
  ul
    [ class "mdl-list" ]
    (map listItem items)

listItem : ListItem -> Html
listItem item =
  li
    [ class "mdl-list__item mdl-list__item--three-line" ]
    [ span
        [ class "mdl-list__item-primary-content" ]
        [ itemIcon item.icon
        , span [] [ text item.title ]
        , span
            [ class "mdl-list__item-text-body" ]
            [ text item.body ]
        ]
    , span
        [ class "mdl-list__item-secondary-content" ]
        [ a [ class "mdl-list__item-secondary-action" ]
            [ secondaryIcon item.secondaryIcon ]
        ]
    ]

secondaryIcon : Icon -> Html
secondaryIcon icon =
  case icon of
    NoIcon ->
      span [] []
    Star ->
      i [ class "material-icons" ] [ text "star" ]

itemIcon : ListIcon -> Html
itemIcon icon =
  case icon of
    Avatar ->
      i [ class "material-icons mdl-list__item-avatar" ] [ text "avatar" ]
    NoListIcon ->
      span [] []
