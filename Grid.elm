module Grid
  ( grid
  ) where

import Styles exposing (..)
import List exposing (map, length)

type alias ColumnDef a = { name : String, fn : a -> String }

grid : List (ColumnDef a) -> List a -> Node
grid columns data =
  nodes (List.append
           [ cardHeader "Food Log"
           , headerRow columns ]
           (map (gridRow columns) data))

cardHeader : String -> Node
cardHeader title =
  nodes [text title]
    |> withStyles [ Flex
                  , AlignItems AlignCenter
                  , Height (Px 64) ]

headerRow : List (ColumnDef a) -> Node
headerRow columns =
  let pct = 1.0 / toFloat (length columns)
  in columns
    |> map (\{name} -> gridCol name |> withStyle (Width (Pct pct)))
    |> nodes
    |> withStyles [ Flex
                  , SpaceBetween
                  , AlignItems AlignCenter
                  , Height (Px 56) ]

gridRow : List (ColumnDef a) -> a -> Node
gridRow columns obj =
  let pct = 1.0 / toFloat (length columns)
  in columns
    |> map (\{fn} -> gridCol (fn obj) |> withStyle (Width (Pct pct)))
    |> nodes
    |> withStyles [ Flex
                  , SpaceBetween
                  , AlignItems AlignCenter
                  , Height (Px 48) ]



gridCol : String -> Node
gridCol val =
  nodes [text val]
