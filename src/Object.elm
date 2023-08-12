module Object exposing
  ( Object(..)
  , ContactReaction(..)
  , reaction
  , isFerromagnet
  , chipSize
  , toSvg
  , toChar
  , fromChar
  )

import Axis exposing (Axis(..))
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
 | Magnet Axis



-- PROPERTIES --

type ContactReaction
 = Takable
 | Movable
 | Fixed
 | Aggressive

reaction : Object -> ContactReaction
reaction obj =
  case obj of
    Paku               -> Fixed
    Wall               -> Fixed
    Gem _ _            -> Takable
    Block              -> Movable
    Kiki _             -> Movable
    ClockwiseBlock     -> Movable
    AntiClockwiseBlock -> Movable
    CrackedBlock       -> Takable
    Spinner _          -> Aggressive
    Pusher _ _         -> Movable
    Magnet _           -> Movable

isFerromagnet : Axis -> Object -> Bool
isFerromagnet magAxis obj =
  case obj of
    Block              -> True
    Kiki _             -> True
    ClockwiseBlock     -> True
    AntiClockwiseBlock -> True
    Spinner _          -> True
    Pusher _ _         -> True
    Magnet axis        -> axis /= magAxis
    _                  -> False



-- VIEW --

chipSize = 16

toSvg : Int -> Int -> Object -> Svg msg
toSvg chipOffsetX chipOffsetY obj =
  obj
    |> tofigureList
    |> List.map (translate (toFloat (chipOffsetX * chipSize)) (toFloat (chipOffsetY * chipSize)))
    |> List.map figureToSvg
    |> Svg.g []

tofigureList : Object -> List Figure
tofigureList obj =
  case obj of
    Paku ->
      [ Circle 8.0 8.0 6.0 black green ]

    Wall ->
      [ Rectangle 0.0 0.0 16.0 16.0 none lightGray
      , Rectangle 4.0 4.0 8.0 8.0 white none
      ]

    Gem d _ ->
      directional d
      [ Polygon [(0.0, 8.0), (8.0, 8.0), (8.0, 0.0)] none lightBlue
      , Polygon [(8.0, 0.0), (8.0, 8.0), (16.0, 8.0)] none darkBlue
      , Polygon [(16.0, 8.0), (8.0, 8.0), (8.0, 16.0)] none blue
      , Polygon [(8.0, 16.0), (8.0, 8.0), (0.0, 8.0)] none darkBlue
      ]

    Block ->
      [ Rectangle 1.0 1.0 14.0 14.0 black yellow ]

    Kiki d ->
      directional d
      [ Rectangle 1.0 1.0 14.0 14.0 black yellow
      , Polyline [(8.0, 13.0), (8.0, 3.0), (3.0, 8.0), (13.0, 8.0), (8.0, 3.0)] red
      ]

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

    Pusher d _ ->
      directional d
      [ Rectangle 2.0 7.0 12.0 6.0 red yellow
      , Polygon [(8.0, 2.0), (14.0, 7.0), (2.0, 7.0)] none red
      , Rectangle 6.0 7.0 4.0 6.0 none red
      ]

    Magnet axis ->
      let
        verticalFig =
          [ Polygon [(0, 3), (1, 1), (15, 1), (16, 3), (15, 5), (1, 5)] black red
          , Rectangle 4 5 8 6 black yellow
          , Polygon [(0, 13), (1, 11), (15, 11), (16, 13), (15, 15), (1, 15)] black red
          ]
      in
        case axis of
          Horizontal -> verticalFig |> List.map (rotate 90)
          Vertical   -> verticalFig

directional d figList =
  case d of
    Up    -> figList
    Down  -> figList |> List.map (rotate 180)
    Left  -> figList |> List.map (rotate 270)
    Right -> figList |> List.map (rotate 90)



-- SERIALIZE --

toChar : Maybe Object -> Char
toChar obj =
  case obj of
    Just Paku                -> '@'
    Just Wall                -> 'W'
    Just (Gem _ _)           -> 'G'
    Just Block               -> 'B'
    Just (Kiki Up)           -> '8'
    Just (Kiki Down)         -> '2'
    Just (Kiki Left)         -> '4'
    Just (Kiki Right)        -> '6'
    Just ClockwiseBlock      -> ','
    Just AntiClockwiseBlock  -> ';'
    Just CrackedBlock        -> 'C'
    Just (Spinner _)         -> '+'
    Just (Pusher Up _)       -> '^'
    Just (Pusher Down _)     -> 'v'
    Just (Pusher Left _)     -> '<'
    Just (Pusher Right _)    -> '>'
    Just (Magnet Vertical)   -> '|'
    Just (Magnet Horizontal) -> '-'
    Nothing                  -> ' '

fromChar : Char -> Maybe Object
fromChar c =
  case c of
    '@' -> Just Paku
    'W' -> Just Wall
    'G' -> Just (Gem Up 0)
    'B' -> Just Block
    '8' -> Just (Kiki Up)
    '2' -> Just (Kiki Down)
    '4' -> Just (Kiki Left)
    '6' -> Just (Kiki Right)
    ',' -> Just ClockwiseBlock
    ';' -> Just AntiClockwiseBlock
    'C' -> Just CrackedBlock
    '+' -> Just (Spinner 0)
    '^' -> Just (Pusher Up 0)
    'v' -> Just (Pusher Down 0)
    '<' -> Just (Pusher Left 0)
    '>' -> Just (Pusher Right 0)
    '|' -> Just (Magnet Vertical)
    '-' -> Just (Magnet Horizontal)
    _ -> Nothing



-- FIGURES --

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
