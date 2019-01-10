module Mapchip exposing
 ( Mapchip(..)
 , Movility(..)
 , chipSize
 , toSvg
 , movility
 )

import Svg exposing (..)
import Svg.Attributes exposing (..)

type Mapchip
 = Paku
 | Wall
 | Gem
 | Block

type Movility
 = Takable
 | Movable
 | Fixed

chipSize = 16

toSvg: Int -> Int -> Mapchip -> List (Svg msg)
toSvg chipOffsetX chipOffsetY obj =
  obj
    |> tofigureList
    |> List.map (figureToSvg (chipOffsetX * chipSize) (chipOffsetY * chipSize))

movility: Mapchip -> Movility
movility mapchip =
  case mapchip of
    Paku -> Fixed
    Wall -> Fixed
    Gem -> Takable
    Block -> Movable

tofigureList: Mapchip -> List Figure
tofigureList obj =
  case obj of
    Paku ->
      [ Circle 8 8 6 black yellow ]
    Wall ->
      [ Rectangle 0 0 16 16 none lightGray
      , Rectangle 4 4 8 8 white none
      ]
    Gem ->
      [ Polygon [(0, 8), (8, 8), (8, 0)] none blue
      , Polygon [(8, 0), (8, 8), (16, 8)] none darkblue
      , Polygon [(16, 8), (8, 8), (8, 16)] none blue
      , Polygon [(8, 16), (8, 8), (0, 8)] none darkblue
      ]
    Block ->
      [ Rectangle 1 1 14 14 black yellow ]

type Figure
 = Rectangle Int Int Int Int Color Color
 | Polygon (List (Int, Int)) Color Color
 | Polyline (List (Int, Int)) Color
 | Circle Int Int Int Color Color

figureToSvg offsetX offsetY obj =
  case obj of
    Rectangle x_ y_ w_ h_ s_ f_ ->
      rect
        [ x <| String.fromInt <| offsetX + x_
        , y <| String.fromInt <| offsetY + y_
        , width <| String.fromInt <| w_
        , height <| String.fromInt <| h_
        , stroke s_
        , fill f_
        ][]

    Polygon pl s_ f_ ->
      polygon
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt (offsetX + x_) ++ "," ++ String.fromInt (offsetY + y_))
            |> List.intersperse " "
            |> String.concat)
        , stroke s_
        , fill f_
        ][]

    Polyline pl c_ ->
      polygon
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt (offsetX + x_) ++ "," ++ String.fromInt (offsetY + y_))
            |> List.intersperse " "
            |> String.concat)
        , stroke c_
        , fill "none"
        ][]

    Circle x_ y_ r_ s_ f_ ->
      circle
        [ cx <| String.fromInt <| offsetX + x_
        , cy <| String.fromInt <| offsetY + y_
        , r <| String.fromInt <| r_
        , stroke s_
        , fill f_
        ][]

type alias Color = String

none = "none"

red = "#FF0000"
yellow = "#FFFF00"
green = "#00FF00"
blue = "#0000FF"
darkblue = "#000088"

white = "#FFFFFF"
lightGray = "#AAAAAA"
gray = "#888888"
black = "#000000"
