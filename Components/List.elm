module Components.List
  ( Icon(..)
  , list
  ) where

import Styles exposing (..)
import List exposing (map, length)

type Icon = None
          | Chicken
          | Burrito

type alias ListItem =
  { icon : Icon
  , mainText : String
  , subText : String
  , minorText : String
  }

list : List ListItem -> Node
list items =
  items
    |> map row
    |> nodes

subTextStyle : Style
subTextStyle =
  [ Font Small
  , Color Grey
  ]

row : ListItem -> Node
row item =
  let mainItem = nodes [ nodes [ text item.mainText ]
                       , nodes [ text item.subText ] |> withStyles subTextStyle ]
         |> withStyles [ PaddingLeft (Px 26) ]
  in nodes [ toIconNode item.icon
           , mainItem
           ]
    |> withStyles [ Flex
                  , AlignItems AlignCenter
                  , PaddingTop (Px 16)
                  , PaddingLeft (Px 24)
                  , PaddingRight (Px 24)
                  , PaddingBottom (Px 16)
                  , Border [ Bottom ] (Px 1) LightGrey
                  ]

toIconNode : Icon -> Node
toIconNode icon =
  let extraStyle =
        case icon of
          None -> [ BackgroundColor Grey ]
          Chicken -> [ BackgroundImage "images/chicken-icon.png"
                     , BackgroundSize Cover ]
          Burrito -> [ BackgroundImage "images/burrito.png"
                     , BackgroundSize Cover ]
  in nodes []
    |> withStyles ([ Width (Px 32)
                   , Height (Px 32)
                   , Rounded (Pct 0.5)
                   ] ++ extraStyle)
