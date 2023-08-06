module Object exposing
  ( Object(..)
  , ContactReaction(..)
  , chipSize
  , toSvg
  , touchArea
  , reaction
  , fadeOut
  )

import Direction exposing (Direction(..))

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown)
import Html.Events.Extra.Touch exposing (onStart)

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

toSvg : Int -> Int -> Object -> Svg msg
toSvg chipOffsetX chipOffsetY obj =
  obj
    |> tofigureList
    |> List.map (translate (toFloat (chipOffsetX * chipSize)) (toFloat (chipOffsetY * chipSize)))
    |> List.map figureToSvg
    |> Svg.g []

touchArea : Int -> Int -> msg -> Svg msg
touchArea chipOffsetX chipOffsetY msg =
  rect
    [ x <| String.fromInt <| (chipOffsetX*chipSize)
    , y <| String.fromInt <| (chipOffsetY*chipSize)
    , width <| String.fromInt <| chipSize
    , height <| String.fromInt <| chipSize
    , stroke "none"
    , fill "#00000010"
    , onMouseDown (msg)
    , onStart (\_ -> msg)
    ][]

reaction : Object -> ContactReaction
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


tofigureList : Object -> List Figure
tofigureList obj =
  case obj of
    Paku ->
      [ Circle 8.0 8.0 6.0 black green ]

    Wall ->
      [ Rectangle 0.0 0.0 16.0 16.0 none lightGray
      , Rectangle 4.0 4.0 8.0 8.0 white none
      ]

    Gem Up _ ->
      [ Polygon [(0.0, 8.0), (8.0, 8.0), (8.0, 0.0)] none lightBlue
      , Polygon [(8.0, 0.0), (8.0, 8.0), (16.0, 8.0)] none darkBlue
      , Polygon [(16.0, 8.0), (8.0, 8.0), (8.0, 16.0)] none blue
      , Polygon [(8.0, 16.0), (8.0, 8.0), (0.0, 8.0)] none darkBlue
      ]
    Gem Down _ ->
      Gem Up 0 |> tofigureList |> List.map (rotate 180)
    Gem Left _->
      Gem Up 0 |> tofigureList |> List.map (rotate 270)
    Gem Right _->
      Gem Up 0 |> tofigureList |> List.map (rotate 90)

    Block ->
      [ Rectangle 1.0 1.0 14.0 14.0 black yellow ]

    Kiki Up ->
      [ Rectangle 1.0 1.0 14.0 14.0 black yellow
      , Polyline [(8.0, 13.0), (8.0, 3.0), (3.0, 8.0), (13.0, 8.0), (8.0, 3.0)] red
      ]
    Kiki Down ->
      Kiki Up |> tofigureList |> List.map (rotate 180)
    Kiki Left ->
      Kiki Up |> tofigureList |> List.map (rotate 270)
    Kiki Right ->
      Kiki Up |> tofigureList |> List.map (rotate 90)

    ClockwiseBlock ->
      [ Rectangle 1.0 1.0 14.0 14.0 black yellow
      , Polyline [(11.0, 11.0), (5.0, 11.0), (5.0, 5.0), (11.0, 5.0), (8.0, 8.0)] red
      ]

    AntiClockwiseBlock ->
      ClockwiseBlock |> tofigureList |> List.map mirrorX

    CrackedBlock ->
      [ Rectangle 1.0 1.0 14.0 14.0 gray lightYellow ]

    Spinner i ->
      [ Polygon
        [ ( 5.0,  1.0), (11.0,  1.0), (11.0,  3.0), ( 9.0,  3.0), (9.0, 7.0), (13.0,  7.0), (13.0,  5.0)
        , (15.0,  5.0), (15.0, 11.0), (13.0, 11.0), (13.0,  9.0), (9.0, 9.0), ( 9.0, 13.0), (11.0, 13.0)
        , (11.0, 15.0), ( 5.0, 15.0), ( 5.0, 13.0), ( 7.0, 13.0), (7.0, 9.0), ( 3.0,  9.0), ( 3.0, 11.0)
        , ( 1.0, 11.0), ( 1.0,  5.0), ( 3.0,  5.0), ( 3.0,  7.0), (7.0, 7.0), ( 7.0,  3.0), ( 5.0,  3.0)
        ] none black
      , Circle 8.0 8.0 3.0 black magenta
      ]
        |> List.map (rotate (toFloat i * 30.0))

    Pusher Up _ ->
      [ Rectangle 2.0 7.0 12.0 6.0 red yellow
      , Polygon [(8.0, 2.0), (14.0, 7.0), (2.0, 7.0)] none red
      , Rectangle 6.0 7.0 4.0 6.0 none red
      ]
    Pusher Down _ ->
      Pusher Up 0 |> tofigureList |> List.map (rotate 180)
    Pusher Left _ ->
      Pusher Up 0 |> tofigureList |> List.map (rotate 270)
    Pusher Right _ ->
      Pusher Up 0 |> tofigureList |> List.map (rotate 90)



type Figure
  = Rectangle Float Float Float Float Color Color
  | Polygon (List (Float, Float)) Color Color
  | Polyline (List (Float, Float)) Color
  | Circle Float Float Float Color Color

figureToSvg obj =
  case obj of
    Rectangle x_ y_ w_ h_ s_ f_ ->
      rect
        [ x <| String.fromFloat <| x_
        , y <| String.fromFloat <| y_
        , width <| String.fromFloat <| w_
        , height <| String.fromFloat <| h_
        , stroke s_
        , fill f_
        ][]

    Polygon pl s_ f_ ->
      polygon
        [ points (
          pl
            |> List.map (\(x_, y_) -> String.fromFloat x_ ++ "," ++ String.fromFloat y_)
            |> List.intersperse " "
            |> String.concat)
        , stroke s_
        , fill f_
        ][]

    Polyline pl c_ ->
      polyline
        [ points (
          pl
            |> List.map (\(x_, y_) -> String.fromFloat x_ ++ "," ++ String.fromFloat y_)
            |> List.intersperse " "
            |> String.concat)
        , stroke c_
        , fill "none"
        ][]

    Circle x_ y_ r_ s_ f_ ->
      circle
        [ cx <| String.fromFloat <| x_
        , cy <| String.fromFloat <| y_
        , r <| String.fromFloat <| r_
        , stroke s_
        , fill f_
        ][]

translate : Float -> Float -> Figure -> Figure
translate offsetX offsetY figure =
  case figure of
    Rectangle x_ y_ w_ h_ s_ f_ ->
      let
        (x__, y__) = translatePoint offsetX offsetY (x_, y_)
      in
        Rectangle x__ y__ w_ h_ s_ f_

    Polygon pl s_ f_ ->
      Polygon (pl |> List.map (translatePoint offsetX offsetY)) s_ f_

    Polyline pl s_ ->
      Polyline (pl |> List.map (translatePoint offsetX offsetY)) s_

    Circle x_ y_ r_ s_ f_ ->
      let
        (x__, y__) = translatePoint offsetX offsetY (x_, y_)
      in
        Circle x__ y__ r_ s_ f_

translatePoint offsetX offsetY (x_, y_) =
  (x_ + offsetX, y_ + offsetY)

mirrorX : Figure -> Figure
mirrorX figure =
  case figure of
    Rectangle x_ y_ w_ h_ s_ f_ ->
      let
        pl = [ (x_, y_), (x_ + w_, y_),(x_ + w_, y_ + h_),(x_, y_ + h_) ]
      in
        Polygon pl s_ f_
          |> mirrorX

    Polygon pl s_ f_ ->
      Polygon (pl |> List.map mirrorXPoint) s_ f_

    Polyline pl s_ ->
      Polyline (pl |> List.map mirrorXPoint) s_

    Circle x_ y_ r_ s_ f_ ->
      let
        (x__, y__) = mirrorXPoint (x_, y_)
      in
        Circle x__ y__ r_ s_ f_

mirrorXPoint (x_, y_) =
  (chipSize - x_, y_)

rotate : Float -> Figure -> Figure
rotate degree figure =
  case figure of
    Rectangle x_ y_ w_ h_ s_ f_ ->
      let
        pl = [ (x_, y_), (x_ + w_, y_),(x_ + w_, y_ + h_),(x_, y_ + h_) ]
      in
        Polygon pl s_ f_
          |> rotate degree

    Polygon pl s_ f_ ->
      Polygon (pl |> List.map (rotatePoint degree)) s_ f_

    Polyline pl s_ ->
      Polyline (pl |> List.map (rotatePoint degree)) s_

    Circle x_ y_ r_ s_ f_ ->
      let
        (x__, y__) = rotatePoint degree (x_, y_)
      in
        Circle x__ y__ r_ s_ f_

rotatePoint deg (x_, y_) =
  let
    rad = degrees deg
    sin_ = sin rad
    cos_ = cos rad
    halfChip = 0.5 * chipSize
  in
    ( (x_ - halfChip) * cos_ - (y_ - halfChip) * sin_ + halfChip
    , (x_ - halfChip) * sin_ + (y_ - halfChip) * cos_ + halfChip
    )

fadeOut : Svg msg -> Svg msg
fadeOut content =
  Svg.g
    [ class "fade-out" ]
    [ content ]

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
