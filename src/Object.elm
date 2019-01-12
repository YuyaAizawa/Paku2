module Object exposing
 ( Object(..)
 , Movility(..)
 , chipSize
 , toSvg
 , movility
 )

import Direction exposing (Direction(..))

import Svg exposing (..)
import Svg.Attributes exposing (..)



type Object
 = Paku
 | Wall
 | {-
      * frame : Direction = アニメーション
      * remaing : Int = 残りのフレーム数
   -}
   Gem Direction Int
 | Block
 | {-
      * direction : Direction = Kikiの向いている方向
   -}
   Kiki Direction
 | ClockwiseBlock
 | AntiClockwiseBlock
 | CrackedBlock


type Movility
 = Takable
 | Movable
 | Fixed

chipSize = 16

toSvg: Int -> Int -> Object -> List (Svg msg)
toSvg chipOffsetX chipOffsetY obj =
  obj
    |> tofigureList
    |> List.map (figureToSvg (chipOffsetX * chipSize) (chipOffsetY * chipSize))

movility: Object -> Movility
movility obj =
  case obj of
    Paku -> Fixed
    Wall -> Fixed
    Gem _ _ -> Takable
    Block -> Movable
    Kiki _ -> Movable
    ClockwiseBlock -> Movable
    AntiClockwiseBlock -> Movable
    CrackedBlock -> Takable

tofigureList: Object -> List Figure
tofigureList obj =
  case obj of
    Paku ->
      [ Circle 8 8 6 black green ]

    Wall ->
      [ Rectangle 0 0 16 16 none lightGray
      , Rectangle 4 4 8 8 white none
      ]

    Gem Up _ ->
      [ Polygon [(0, 8), (8, 8), (8, 0)] none lightBlue
      , Polygon [(8, 0), (8, 8), (16, 8)] none darkBlue
      , Polygon [(16, 8), (8, 8), (8, 16)] none blue
      , Polygon [(8, 16), (8, 8), (0, 8)] none darkBlue
      ]
    Gem Down _ ->
      Gem Up 0 |> tofigureList |> rotate |> rotate
    Gem Left _->
      Gem Up 0 |> tofigureList |> rotate |> rotate |> rotate
    Gem Right _->
      Gem Up 0 |> tofigureList |> rotate

    Block ->
      [ Rectangle 1 1 14 14 black yellow ]

    Kiki Up ->
      [ Rectangle 1 1 14 14 black yellow
      , Polyline [(8, 13), (8, 3), (3, 8), (13, 8), (8, 3)] red
      ]
    Kiki Down ->
      Kiki Up |> tofigureList |> rotate |> rotate
    Kiki Left ->
      Kiki Up |> tofigureList |> rotate |> rotate |> rotate
    Kiki Right ->
      Kiki Up |> tofigureList |> rotate

    ClockwiseBlock ->
      [ Rectangle 1 1 14 14 black yellow
      , Polyline [(11, 11), (5, 11), (5, 5), (11, 5), (8, 8)] red
      ]

    AntiClockwiseBlock ->
      ClockwiseBlock |> tofigureList |> mirrorX

    CrackedBlock ->
      [ Rectangle 1 1 14 14 gray lightYellow ]

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
      polyline
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

mirrorX: List Figure -> List Figure
mirrorX figures =
  let
    mirrorX_ figure =
      case figure of
        Rectangle x_ y_ w_ h_ s_ f_ ->
          Rectangle (16 - (x_ + w_)) y_ w_ h_ s_ f_

        Polygon pl s_ f_ ->
          Polygon (pl |> List.map (\(x_, y_) -> (16 - x_, y_))) s_ f_

        Polyline pl c_ ->
          Polyline (pl |> List.map (\(x_, y_) -> (16 - x_, y_))) c_

        Circle x_ y_ r_ s_ f_ ->
          Circle (16 - x_) y_ r_ s_ f_
  in
    figures |> List.map mirrorX_

rotate: List Figure -> List Figure
rotate figures =
  let
    rotate_ figure =
      case figure of
        Rectangle x_ y_ w_ h_ s_ f_ ->
          Rectangle (chipSize - (y_ + h_)) x_ h_ w_ s_ f_

        Polygon pl s_ f_ ->
          Polygon (pl |> List.map (\(x_, y_) -> (chipSize - y_ , x_))) s_ f_

        Polyline pl c_ ->
          Polyline (pl |> List.map (\(x_, y_) -> (chipSize - y_, x_))) c_

        Circle x_ y_ r_ s_ f_ ->
          Circle (y_- chipSize) x_ r_ s_ f_
  in
    figures |> List.map rotate_

type alias Color = String

none = "none"

red = "#FF0000"
yellow = "#FFFF00"
lightYellow = "#FFFF88"
green = "#00FF00"
blue = "#0000FF"
lightBlue = "#8888FF"
darkBlue = "#000088"

white = "#FFFFFF"
lightGray = "#AAAAAA"
gray = "#888888"
black = "#000000"