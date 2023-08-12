module Axis exposing (..)

import Direction exposing (Direction(..))

type Axis
  = Vertical
  | Horizontal

toDirections : Axis -> List Direction
toDirections axis =
  case axis of
    Vertical   -> [ Up, Down ]
    Horizontal -> [ Left, Right ]
