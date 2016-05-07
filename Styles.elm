module Styles
  ( Node
  , Distance(..)
  , StyleAttr(..)
  , Align(..)
  , withStyle
  , withStyles
  , text
  , nodes
  , render
  ) where

import Html exposing (Html)
import Html.Attributes

import List exposing (map, foldl)

type alias Style = List StyleAttr

type Align = Top
           | Center
           | Bottom

type StyleAttr = Padding Distance
               | Margin Distance
               | Flex
               | SpaceBetween
               | AlignItems Align
               | Grow Float
               | Width Distance
               | Height Distance
               | FontSize Distance
               | Rounded Distance
               | Elevation Int

type Node = Text Style String | Node Style (List Node)


type Distance = Px Int
             | Pct Float

nodes : List Node -> Node
nodes = Node []

text : String -> Node
text val = Text [] val

spacingStr : Distance -> String
spacingStr spacing =
  case spacing of
    Px px -> toString px ++ "px"
    Pct amt -> toString (amt * 100) ++ "%"

alignStr : Align -> String
alignStr direction =
  case direction of
    Top -> "top"
    Center -> "center"
    Bottom -> "bottom"

elevationStr : Int -> String
elevationStr dist =
  let radius = dist * 2
  in "0px " ++ toString dist ++ "px " ++ toString radius ++ "px rgba(0,0,0,0.2)"

style : StyleAttr -> (String, String)
style attr =
  case attr of
    Padding ems -> ("padding", spacingStr ems)
    Margin ems -> ("margin", spacingStr ems)
    Flex -> ("display", "flex")
    SpaceBetween -> ("justifyContent", "space-between")
    AlignItems dir -> ("alignItems", alignStr dir)
    Grow amount -> ("flex-grow", toString amount)
    Rounded radius -> ("border-radius", spacingStr radius)
    Elevation amount -> ("box-shadow", elevationStr amount)
    Width amount -> ("width", spacingStr amount)
    Height amount -> ("height", spacingStr amount)
    FontSize height -> ("fontSize", spacingStr height)

hStyle = Html.Attributes.style

withStyles : List StyleAttr -> Node -> Node
withStyles attrs node = foldl withStyle node attrs

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
