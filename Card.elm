module Card
  ( card
  ) where

import Styles exposing (..)

card : List Node -> Node
card contents =
  contents
    |> nodes
    |> withStyles [ Rounded (Px 2)
                  , Elevation 4
                  , Margin (Px 24)
                  , Border [ Top ] (Px 1) LightGrey]
