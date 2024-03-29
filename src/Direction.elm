module Direction exposing (..)

type Direction
 = Up
 | Down
 | Left
 | Right

values : List Direction
values =
  [ Up
  , Down
  , Left
  , Right
  ]

rotateClockwise direction =
  case direction of
    Up -> Right
    Down -> Left
    Left -> Up
    Right -> Down

rotateAntiClockwise direction =
  case direction of
    Up -> Left
    Down -> Right
    Left -> Down
    Right -> Up

mirror direction =
  case direction of
    Up -> Down
    Down -> Up
    Left -> Right
    Right -> Left
