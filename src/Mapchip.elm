module Mapchip exposing
 ( Mapchip(..)
 , Movility(..)
 , chipSize
 , toSvg
 , movility
 , playerSvg
 )

import Svg exposing (..)
import Svg.Attributes exposing (..)

type Mapchip
 = Wall
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
    Wall -> Fixed
    Gem -> Takable
    Block -> Movable

tofigureList: Mapchip -> List Figure
tofigureList obj =
  case obj of
    Wall ->
      [ FillRect 0 0 16 16 lightGray
      , StrokeRect 4 4 8 8 white
      ]
    Gem ->
      [ Polygon [(0, 8), (8, 8), (8, 0)] blue
      , Polygon [(8, 0), (8, 8), (16, 8)] blue
      , Polygon [(16, 8), (8, 8), (8, 16)] blue
      , Polygon [(8, 16), (8, 8), (0, 8)] blue
      ]
    Block ->
      [ FillRect 1 1 14 14 yellow
      , StrokeRect 1 1 14 14 black
      ]

playerSvg: Int -> Int -> Svg msg
playerSvg chipOffsetX chipOffsetY =
  circle
    [ cx <| String.fromInt <| chipOffsetX * chipSize + 8
    , cy <| String.fromInt <| chipOffsetY * chipSize + 8
    , r "6"
    , stroke black
    , fill green
    ][]

type Figure
 = FillRect Int Int Int Int Color
 | StrokeRect Int Int Int Int Color
 | Polygon (List (Int, Int)) Color

figureToSvg offsetX offsetY obj =
  case obj of
    FillRect x_ y_ w_ h_ c_ ->
      rect
        [ x <| String.fromInt <| offsetX + x_
        , y <| String.fromInt <| offsetY + y_
        , width <| String.fromInt <| w_
        , height <| String.fromInt <| h_
        , stroke "none"
        , fill <| c_
        ][]

    StrokeRect x_ y_ w_ h_ c_ ->
      rect
        [ x <| String.fromInt <| offsetX + x_
        , y <| String.fromInt <| offsetY + y_
        , width <| String.fromInt <| w_
        , height <| String.fromInt <| h_
        , stroke <| c_
        , fill "none"
        ][]

    Polygon pl c_ ->
      polygon
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt (offsetX + x_) ++ "," ++ String.fromInt (offsetY + y_))
            |> List.intersperse " "
            |> String.concat)
        , fill c_
        ][]

type alias Color = String

yellow = "#FFFF00"
green = "#00FF00"
blue = "#0000FF"

white = "#FFFFFF"
lightGray = "#AAAAAA"
gray = "#888888"
black = "#000000"
