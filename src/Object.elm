module Object exposing
 ( Object(..)
 , ContactReaction(..)
 , chipSize
 , toSvg
 , reaction
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
      * direction : Direction = 向いている方向
   -}
   Kiki Direction
 | ClockwiseBlock
 | AntiClockwiseBlock
 | CrackedBlock
 | {-
      * frame : Int アニメーション
   -}
   Spinner Int
 | {-
      * direction : Direction = 向いている方向
      * wait : Int = 移動までの待ち時間
   -}
   Pusher Direction Int


type ContactReaction
 = Takable
 | Movable
 | Fixed
 | Aggressive

chipSize = 16

toSvg: Int -> Int -> Object -> Svg msg
toSvg chipOffsetX chipOffsetY obj =
  obj
    |> tofigureList
    |> List.map figureToSvg
    |> translate (chipOffsetX * chipSize) (chipOffsetY * chipSize)

reaction: Object -> ContactReaction
reaction obj =
  case obj of
    Paku -> Fixed
    Wall -> Fixed
    Gem _ _ -> Takable
    Block -> Movable
    Kiki _ -> Movable
    ClockwiseBlock -> Movable
    AntiClockwiseBlock -> Movable
    CrackedBlock -> Takable
    Spinner _ -> Aggressive
    Pusher _ _ -> Movable


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
      [ Gem Up 0 |> tofigureList |> Rotate 180 ]
    Gem Left _->
      [ Gem Up 0 |> tofigureList |> Rotate 270 ]
    Gem Right _->
      [ Gem Up 0 |> tofigureList |> Rotate 90 ]

    Block ->
      [ Rectangle 1 1 14 14 black yellow ]

    Kiki Up ->
      [ Rectangle 1 1 14 14 black yellow
      , Polyline [(8, 13), (8, 3), (3, 8), (13, 8), (8, 3)] red
      ]
    Kiki Down ->
      [ Kiki Up |> tofigureList |> Rotate 180 ]
    Kiki Left ->
      [ Kiki Up |> tofigureList |> Rotate 270 ]
    Kiki Right ->
      [ Kiki Up |> tofigureList |> Rotate 90 ]

    ClockwiseBlock ->
      [ Rectangle 1 1 14 14 black yellow
      , Polyline [(11, 11), (5, 11), (5, 5), (11, 5), (8, 8)] red
      ]

    AntiClockwiseBlock ->
      [ ClockwiseBlock |> tofigureList |> MirrorX ]

    CrackedBlock ->
      [ Rectangle 1 1 14 14 gray lightYellow ]

    Spinner i ->
      [ Polygon
        [ ( 5,  1), (11,  1), (11,  3), ( 9,  3), (9, 7), (13,  7), (13,  5)
        , (15,  5), (15, 11), (13, 11), (13,  9), (9, 9), ( 9, 13), (11, 13)
        , (11, 15), ( 5, 15), ( 5, 13), ( 7, 13), (7, 9), ( 3,  9), ( 3, 11)
        , ( 1, 11), ( 1,  5), ( 3,  5), ( 3,  7), (7, 7), ( 7,  3), ( 5,  3)
        ] none black
      , Circle 8 8 3 black magenta
      ]
        |> Rotate (i * 30)
        |> List.singleton

    Pusher Up _ ->
      [ Rectangle 2 7 12 6 red yellow
      , Polygon [(8, 2), (14, 7), (2, 7)] none red
      , Rectangle 6 7 4 6 none red
      ]
    Pusher Down _ ->
      [ Pusher Up 0 |> tofigureList |> Rotate 180 ]
    Pusher Left _ ->
      [ Pusher Up 0 |> tofigureList |> Rotate 270 ]
    Pusher Right _ ->
      [ Pusher Up 0 |> tofigureList |> Rotate 90 ]

type Figure
 = Rectangle Int Int Int Int Color Color
 | Polygon (List (Int, Int)) Color Color
 | Polyline (List (Int, Int)) Color
 | Circle Int Int Int Color Color
 | Rotate Int (List Figure)
 | MirrorX (List Figure)

figureToSvg obj =
  case obj of
    Rectangle x_ y_ w_ h_ s_ f_ ->
      rect
        [ x <| String.fromInt <| x_
        , y <| String.fromInt <| y_
        , width <| String.fromInt <| w_
        , height <| String.fromInt <| h_
        , stroke s_
        , fill f_
        ][]

    Polygon pl s_ f_ ->
      polygon
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt x_ ++ "," ++ String.fromInt y_)
            |> List.intersperse " "
            |> String.concat)
        , stroke s_
        , fill f_
        ][]

    Polyline pl c_ ->
      polyline
        [points (
          pl
            |> List.map (\(x_, y_) -> String.fromInt x_ ++ "," ++ String.fromInt y_)
            |> List.intersperse " "
            |> String.concat)
        , stroke c_
        , fill "none"
        ][]

    Circle x_ y_ r_ s_ f_ ->
      circle
        [ cx <| String.fromInt <| x_
        , cy <| String.fromInt <| y_
        , r <| String.fromInt <| r_
        , stroke s_
        , fill f_
        ][]

    Rotate deg figures ->
      g
        [ transform <| "rotate(" ++ String.fromInt deg ++ ", 8, 8)"]
        (figures |> List.map figureToSvg)

    MirrorX figures ->
      g
        [ transform "scale(-1, 1) translate(-16, 0)" ]
        (figures |> List.map figureToSvg)

translate: Int -> Int -> List (Svg msg) -> Svg msg
translate x y contents =
  Svg.g
    [ transform <| "translate(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")" ]
    contents

type alias Color = String

none = "none"

red = "#FF0000"
yellow = "#FFFF00"
lightYellow = "#FFFF88"
green = "#00FF00"
blue = "#0000FF"
lightBlue = "#8888FF"
darkBlue = "#000088"
magenta = "#FF00FF"

white = "#FFFFFF"
lightGray = "#AAAAAA"
gray = "#888888"
black = "#000000"
