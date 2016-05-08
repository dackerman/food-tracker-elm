module Components.Header
  ( fixedHeaderLayout
  , Link(..)
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)

import List exposing (map)

fixedHeaderLayout : String -> List Link -> List Html -> Html
fixedHeaderLayout title links contents =
  div
    [ class "mdl-layout mdl-js-layout mdl-layout--fixed-header" ]
    [ header
        [ class "mdl-layout__header" ]
        [ div
            [ class "mdl-layout__header-row" ]
            [ layoutTitle title
            , div
                [ class "mdl-layout-spacer" ] []
            , nav
                [ class "mdl-navigation mdl-layout--large-screen-only" ]
                (map navLink links)
            ]
        ]
    , div
        [ class "mdl-layout__drawer" ]
        [ layoutTitle title
        , nav
            [ class "mdl-navigation" ]
            (map navLink links)
        ]
    , main'
        [ class "mdl-layout__content" ]
        [ div
            [ class "page-content" ]
            contents
        ]
    ]

layoutTitle : String -> Html
layoutTitle title =
  span
    [ class "mdl-layout-title" ]
    [ text title ]

navLink : Link -> Html
navLink (Link name url) =
  a [ class "mdl-navigation__link", href url]
    [ text name ]

type Link = Link String String
