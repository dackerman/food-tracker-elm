module Styles
  ( Node
  , Distance(..)
  , StyleAttr(..)
  , Align(..)
  , Color(..)
  , Direction(..)
  , BackgroundSize(..)
  , withStyle
  , withStyles
  , text
  , nodes
  , render
  ) where

import Html exposing (Html)
import Html.Attributes

import List exposing (map, concatMap, foldl)

type alias Style = List StyleAttr

type Align = AlignTop
           | AlignCenter
           | AlignBottom

type Color = Grey
           | LightGrey

type Direction = Top
               | Bottom
               | Left
               | Right
               | All

type StyleAttr = Padding Distance
               | PaddingLeft Distance
               | PaddingRight Distance
               | PaddingTop Distance
               | PaddingBottom Distance
               | Border (List Direction) Distance Color
               | Margin Distance
               | Flex
               | Position Position
               | SpaceBetween
               | AlignItems Align
               | Grow Float
               | Width Distance
               | Height Distance
               | FontSize Distance
               | Rounded Distance
               | Elevation Int
               | BackgroundColor Color
               | BackgroundImage String
               | BackgroundSize BackgroundSize

type BackgroundSize = Cover

type Position = Absolute | Relative


type Node = Text Style String | Node Style (List Node)


type Distance = Px Int
             | Pct Float


nodes : List Node -> Node
nodes = Node []


text : String -> Node
text val = Text [] val


positionStr : Position -> String
positionStr pos = case pos of
                    Absolute -> "absolute"
                    Relative -> "relative"

spacingStr : Distance -> String
spacingStr spacing =
  case spacing of
    Px px -> toString px ++ "px"
    Pct amt -> toString (amt * 100) ++ "%"

alignStr : Align -> String
alignStr direction =
  case direction of
    AlignTop -> "top"
    AlignCenter -> "center"
    AlignBottom -> "bottom"

borderDirectionStr : Direction -> String
borderDirectionStr direction =
  case direction of
    Top -> "border-top"
    Bottom -> "border-bottom"
    Left -> "border-left"
    Right -> "border-right"
    All -> "border"

elevationStr : Int -> String
elevationStr dist =
  let heightPx = toString <| toFloat dist / 2
  in "0px " ++ heightPx ++ "px " ++ heightPx ++ "px rgba(0,0,0,0.2)"

colorStr : Color -> String
colorStr color =
  case color of
    Grey -> "#888"
    LightGrey -> "#eee"

bgSizeStr : BackgroundSize -> String
bgSizeStr size =
  case size of
    Cover -> "cover"

bgImgStr : String -> String
bgImgStr url =
  "url(" ++ url ++ ")"

style : StyleAttr -> List (String, String)
style attr =
  case attr of
    Padding ems -> [("padding", spacingStr ems)]
    PaddingLeft ems -> [("paddingLeft", spacingStr ems)]
    PaddingRight ems -> [("paddingRight", spacingStr ems)]
    PaddingTop ems -> [("paddingTop", spacingStr ems)]
    PaddingBottom ems -> [("paddingBottom", spacingStr ems)]
    Border directions distance color -> borderStyles directions distance color
    Margin ems -> [("margin", spacingStr ems)]
    Flex -> [("display", "flex")]
    Position pos -> [("position", positionStr pos)]
    SpaceBetween -> [("justifyContent", "space-between")]
    AlignItems dir -> [("alignItems", alignStr dir)]
    Grow amount -> [("flex-grow", toString amount)]
    Rounded radius -> [("border-radius", spacingStr radius)]
    Elevation amount -> [("box-shadow", elevationStr amount)]
    Width amount -> [("width", spacingStr amount)]
    Height amount -> [("height", spacingStr amount)]
    FontSize height -> [("fontSize", spacingStr height)]
    BackgroundColor color -> [("backgroundColor", colorStr color)]
    BackgroundImage img -> [("backgroundImage", bgImgStr img)]
    BackgroundSize size -> [("backgroundSize", bgSizeStr size)]


borderStyles : List Direction -> Distance -> Color -> List (String, String)
borderStyles directions distance color =
  let borderProps = spacingStr distance ++ " solid " ++ colorStr color
      border direction = (borderDirectionStr direction, borderProps)
  in map border directions


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
    Text styles val -> Html.div [styleToAttribute styles] [Html.text val]
    Node styles nodes -> Html.div [styleToAttribute styles] (map render nodes)


styleToAttribute : Style -> Html.Attribute
styleToAttribute styles = Html.Attributes.style (concatMap style styles)
