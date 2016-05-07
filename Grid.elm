module Grid
  ( grid
  ) where

import Styles exposing (..)
import List exposing (map)


grid : List (a -> String) -> List a -> Node
grid columns data =
   nodes (map (gridRow columns) data)
     |> withStyle (Margin (Rem 1))


gridRow : List (a -> String) -> a -> Node
gridRow columns obj =
  columns
    |> map ((|>) obj)
    |> map gridCol
    |> nodes


gridCol : String -> Node
gridCol val =
  nodes [text val]
    |> withStyle (Margin (Rem 1))
