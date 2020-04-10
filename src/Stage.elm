module Stage exposing
  ( Stage
  , State(..)
  , put
  , move
  , enemyTurn
  , state
  , view
  , toString
  , fromString
  )

import Direction exposing (Direction(..))
import Object exposing (Object(..), ContactReaction(..))

import Dict exposing (Dict)
import Random exposing (Seed)
import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)



type alias Stage =
  { map : Dict Coord Object
  , size : Coord
  , miss : Bool
  }

type alias Coord = ( Int, Int )

type State
  = Playing
  | Cleared
  | Miss

empty =
  { map =
      Dict.fromList
      [ ( ( 0, 0 ), Paku )
      , ( ( 1, 1 ), Gem Up 0 )
      ]
  , size =
      ( 2, 2 )
  , miss = False
  }

fromDict : Dict Coord Object -> Stage
fromDict map =
  let
    gems =
      map
        |> Dict.values
        |> List.filter (\o -> case o of
          Gem _ _ -> True
          _ -> False)
        |> List.length
    w =
      map
        |> Dict.keys
        |> List.map (\(x, _) -> x)
        |> List.maximum
        |> Maybe.map (\n -> n + 1)
        |> Maybe.withDefault 8
    h =
      map
        |> Dict.keys
        |> List.map (\(_, y) -> y)
        |> List.maximum
        |> Maybe.map (\n -> n + 1)
        |> Maybe.withDefault 6

  in
  { map = map
  , size = ( w, h )
  , miss = False
  }


type EntryType
  = JustEntry
  | PushEntry Object
  | TakeEntry Object
  | CannotEntry
  | CannotEntryAndDamaged


put : Int -> Int -> Maybe Object -> Stage -> Stage
put x y maybe stage =
  let
    map = case maybe of
      Nothing ->
        stage.map |> Dict.remove ( x, y )
      Just obj ->
        if obj == Paku
        then
          stage.map
            |> Dict.remove (pakuPos stage)
            |> Dict.insert ( x, y ) Paku
        else
          stage.map |> Dict.insert ( x, y ) obj
  in
    { stage | map = map }

move : Direction -> Stage -> Stage
move direction stage =
  if stage.miss
  then stage
  else
    let
      p0 = stage |> pakuPos
      p1 = p0    |> towards direction
      p2 = p1    |> towards direction

      o1 = stage.map |> Dict.get p1

      entryType =
        case o1 of
          Nothing ->
            JustEntry

          Just obj ->
            case obj |> Object.reaction of
              Takable ->
                TakeEntry obj

              Movable ->
                  if stage.map |> Dict.member p2
                  then CannotEntry
                  else PushEntry obj

              Fixed ->
                CannotEntry

              Aggressive ->
                CannotEntryAndDamaged

      map =
        case entryType of
          JustEntry ->
            stage.map
              |> Dict.remove p0
              |> Dict.insert p1 Paku

          TakeEntry _ ->
            stage.map
              |> Dict.remove p0
              |> Dict.insert p1 Paku

          PushEntry obj ->
            stage.map
              |> Dict.remove p0
              |> Dict.insert p1 Paku
              |> Dict.insert p2 obj

          _ ->
            stage.map

      miss =
        case entryType of
          CannotEntryAndDamaged -> True
          _                     -> False
    in
      { stage
      | map = map
      , miss = miss
      }


towards direction (x, y) =
  case direction of
        Up ->    (x    , y - 1)
        Down ->  (x    , y + 1)
        Left ->  (x - 1, y    )
        Right -> (x + 1, y    )


enemyTurn : Stage -> Random.Generator Stage
enemyTurn stage =
  let
    acc ( pos, obj ) prev =
      prev
        |> Random.andThen (\stage_ -> step pos obj stage_)

    init =
      Random.constant stage
  in
    stage.map
      |> Dict.toList
      |> List.foldl acc init


step : Coord -> Object -> Stage -> Random.Generator Stage
step pos obj stage =
  case obj of
    Kiki direction ->
      let
        map =
          case stage.map |> Dict.get (pos |> towards direction) of
            Nothing ->
              stage.map |> Dict.remove pos |> Dict.insert (pos |> towards direction) obj
            Just ClockwiseBlock ->
              stage.map |> Dict.insert pos (Kiki (Direction.rotateClockwise direction))
            Just AntiClockwiseBlock ->
              stage.map |> Dict.insert pos (Kiki (Direction.rotateAntiClockwise direction))
            _ -> stage.map
      in
        { stage | map = map } |> Random.constant

    Gem frame 0 ->
      gemGenerator
        |> Random.map (\( nextFrame, nextRemaining ) ->
          let
            map =
              stage.map
                |> Dict.insert pos ( Gem nextFrame nextRemaining )
          in
            { stage | map = map }
        )

    Gem frame remaining ->
      let
        map =
          stage.map
            |> Dict.insert pos (Gem frame (remaining - 1))
      in
        { stage | map = map } |> Random.constant

    Spinner i ->
      let
        nextFrame =
          case i of
            2 -> 0
            n -> n + 1
        nextSpinner = Spinner nextFrame
      in
        spinnerAi pos (pakuPos stage)
          |> Random.map (\direction ->
            let
              nextPos =
                direction
                  |> Maybe.map (\d -> pos |> towards d)
                  |> Maybe.withDefault pos
              removedMap = stage.map |> Dict.remove pos
              ( nextMap, nextDamaged ) =
                case removedMap |> Dict.get nextPos of
                  Nothing -> (removedMap |> Dict.insert nextPos nextSpinner, False)
                  Just Paku -> (removedMap |> Dict.insert pos nextSpinner, True)
                  _ -> (removedMap |> Dict.insert pos nextSpinner, False)
            in
              { stage
              | map = nextMap
              , miss = stage.miss || nextDamaged
              }
          )

    Pusher d 0 ->
      let
        pusherWait = 6
        p1 = towards d pos
        p2 = towards d p1
        o1 = Dict.get p1 stage.map
        entryType =
          o1
            |> Maybe.map
              ( \o -> case Object.reaction o of
                Movable ->
                  if stage.map |> Dict.member p2
                  then CannotEntry
                  else PushEntry o
                Fixed -> CannotEntry
                _ -> CannotEntry
              )
            |> Maybe.withDefault JustEntry

        map =
          case entryType of
            JustEntry ->
              stage.map
                |> Dict.remove pos
                |> Dict.insert p1 (Pusher d pusherWait)
            PushEntry o ->
              stage.map
                |> Dict.remove pos
                |> Dict.insert p2 o
                |> Dict.insert pos (Pusher (d |> Direction.mirror) pusherWait)
            CannotEntry ->
              stage.map
                |> Dict.insert pos (Pusher (d |> Direction.mirror) pusherWait)
            _ -> stage.map
      in
        { stage | map = map } |> Random.constant

    Pusher d n ->
      let
        map = stage.map |> Dict.insert pos (Pusher d (n-1))
      in
        { stage | map = map } |> Random.constant

    _ -> stage |> Random.constant


pakuPos : Stage -> Coord
pakuPos stage =
  stage.map
    |> Dict.toList
    |> List.filter (\( _, obj ) -> obj == Paku)
    |> List.head
    |> Maybe.map Tuple.first
    |> Maybe.withDefault ( -1, -1 )

gemGenerator : Random.Generator (Direction, Int)
gemGenerator =
  Random.pair randomDirecction (Random.int 5 10)

spinnerAi : Coord -> Coord -> Random.Generator (Maybe Direction)
spinnerAi (sx, sy) (px, py) =
  Random.int 0 3
    |> Random.andThen (\i ->
      case i of
        0 ->
          Just (pursuit sx sy px py)
            |> Random.constant
        1 ->
          randomDirecction
            |> Random.map Just
        _ -> Nothing
            |> Random.constant)

pursuit ex ey px py =
  if (px - ex)*(px - ex) > (py - ey)*(py - ey)
  then if px - ex > 0
    then Right
    else Left
  else if py - ey > 0
    then Down
    else Up

randomDirecction : Random.Generator Direction
randomDirecction =
  Random.int 0 3
    |> Random.map (\n ->
      case n of
        0 -> Up
        1 -> Down
        2 -> Left
        _ -> Right)

countGems : Stage -> Int
countGems stage =
  stage.map
    |> Dict.toList
    |> List.filter (\( _, obj ) ->
      case obj of
        Gem _ _ -> True
        _ -> False)
    |> List.length

state : Stage -> State
state stage =
  if stage.miss
  then Miss
  else if countGems stage == 0
  then Cleared
  else Playing

view : Stage -> Html msg
view stage =
  let (px, py) = pakuPos stage in
  Svg.svg []
    ( stage.map
      |> Dict.toList
      |> List.map (\((x, y), obj) ->
        if obj == Paku && stage.miss
        then obj |> Object.toSvg x y |> Object.fadeOut
        else obj |> Object.toSvg x y)
    )


coordsToString (x, y) =
  "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

toString : Stage -> String
toString stage =
  let ( width, height ) = stage.size in
  List.range 0 (height - 1)
    |> List.concatMap (\y -> List.range 0 width
      |> List.map (\x -> (x, y)))
    |> List.map (\(x, y) ->
      if x == width
      then '\n'
      else
        stage.map
          |> Dict.get (x, y)
          |> objToChar)
    |> String.fromList


fromString : String -> Stage
fromString src =
  let
    map =
      src
        |> String.split "\n"
        |> List.indexedMap (\y l ->
          l |> String.toList
            |> List.indexedMap (\x c -> ((x, y), c))
            |> List.filterMap (\(coord, char) ->
              objFromChar char
                |> Maybe.map(\o -> (coord, o))))
        |> List.concat
        |> Dict.fromList
  in
  fromDict map

objToChar : Maybe Object -> Char
objToChar obj =
  case obj of
    Just Paku               -> '@'
    Just Wall               -> 'W'
    Just (Gem _ _)          -> 'G'
    Just Block              -> 'B'
    Just (Kiki Up)          -> '8'
    Just (Kiki Down)        -> '2'
    Just (Kiki Left)        -> '4'
    Just (Kiki Right)       -> '6'
    Just ClockwiseBlock     -> ','
    Just AntiClockwiseBlock -> ';'
    Just CrackedBlock       -> 'C'
    Just (Spinner _)        -> '+'
    Just (Pusher Up _)      -> '^'
    Just (Pusher Down _)    -> 'v'
    Just (Pusher Left _)    -> '<'
    Just (Pusher Right _)   -> '>'
    Nothing                 -> ' '

objFromChar : Char -> Maybe Object
objFromChar c =
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
    _ -> Nothing