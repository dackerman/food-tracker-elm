module Styles
  ( Node(..)
  , Spacing(..)
  , StyleAttr(..)
  , withStyle
  , text
  , nodes
  , render
  ) where

import Html exposing (Html)
import Html.Attributes

import List exposing (map)

type alias Style = List StyleAttr

type StyleAttr = Padding Spacing
               | Margin Spacing

type Node = Text Style String | Node Style (List Node)

type Spacing = Rem Float

nodes : List Node -> Node
nodes = Node []

text : String -> Node
text val = Text [] val

spacingStr : Spacing -> String
spacingStr spacing =
  case spacing of
    Rem rems -> toString rems ++ "rem"

style : StyleAttr -> (String, String)
style attr =
  case attr of
    Padding ems -> ("padding", spacingStr ems)
    Margin ems -> ("margin", spacingStr ems)

hStyle = Html.Attributes.style

withStyle : StyleAttr -> Node -> Node
withStyle attr node =
  case node of
    Text styles val -> Text (attr :: styles) val
    Node styles nodes -> Node (attr :: styles) nodes


render : Node -> Html
render node =
  case node of
    Text [] val -> Html.text val
    Text styles val -> Html.div [hStyle (map style styles)] [Html.text val]
    Node styles nodes -> Html.div [hStyle (map style styles)] (map render nodes)
