module Card
  ( card
  ) where

import Styles exposing (..)

card : List Node -> Node
card contents =
  contents
    |> nodes
    |> withStyles [ Padding (Px 24)
                  , Rounded (Px 2)
                  , Elevation 4
                  , Margin (Px 24)]
