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

import Axis
import Direction exposing (Direction(..))
import Object exposing (Object(..), ContactReaction(..))

import Dict exposing (Dict)
import Random exposing (Seed)
import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes exposing (class, viewBox)



type alias Stage =
  { map : Map
  , size : Coord
  , miss : Bool
  }

type alias Map = Dict Coord Object

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
        stage.map
          |> Dict.remove ( x, y )

      Just Paku ->
        stage.map
          |> Dict.remove (pakuPos stage)
          |> Dict.insert ( x, y ) Paku

      Just obj ->
        stage.map
          |> Dict.insert ( x, y ) obj
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
              |> pullMagnets p1

          TakeEntry _ ->
            stage.map
              |> Dict.remove p0
              |> Dict.insert p1 Paku
              |> pullMagnets p1

          PushEntry obj ->
            stage.map
              |> Dict.remove p0
              |> Dict.insert p1 Paku
              |> Dict.insert p2 obj
              |> pullMagnets p1

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

pullMagnets : Coord -> Map -> Map
pullMagnets paku map =
  Direction.values
    |> List.foldl (pullMagnet paku) map

pullMagnet : Coord -> Direction -> Map -> Map
pullMagnet paku dir map =
  let
    front = paku |> towards dir

    back = paku |> towards dir |> towards dir

    backObj = map |> Dict.get back

    spaceInFront =
      map
        |> Dict.get front
        |> (==) Nothing

    rightAxisMagnet =
      case backObj of
        Just (Magnet axis) ->
          axis
            |> Axis.toDirections
            |> List.member dir
        _ -> False
  in
    if spaceInFront && rightAxisMagnet then
      map
        |> Dict.remove back
        |> Dict.update front (\_ -> backObj)
    else
      map


towards direction ( x, y ) =
  case direction of
    Up ->    ( x    , y - 1 )
    Down ->  ( x    , y + 1 )
    Left ->  ( x - 1, y     )
    Right -> ( x + 1, y     )

enemyTurn : Stage -> Random.Generator Stage
enemyTurn stage =
  stage
    |> magnetsTurn
    |> pushersTurn
    |> miscsTurn

magnetsTurn : Stage -> ( Stage, List Coord )
magnetsTurn stage =
  let
    bounded =
      stage.map
        |> Dict.toList
        |> List.foldl (accBounded stage.map) []

    ( map, bounded_, _ ) =
      stage.map
        |> Dict.toList
        |> List.foldl magnetsStep ( stage.map, bounded, [] )
  in
    ( { stage | map = map }, bounded_ )

pushersTurn : ( Stage, List Coord ) -> ( Stage, List Coord )
pushersTurn ( stage, bounded ) =
  let
    ( map, interfered ) =
      stage.map
        |> Dict.toList
        |> List.foldl pushersStep ( stage.map, bounded )
  in
    ( { stage | map = map }, interfered )

miscsTurn : ( Stage, List Coord ) -> Random.Generator Stage
miscsTurn ( stage, interfered ) =
  let
    acc ( pos, obj ) prev =
      prev
        |> Random.andThen (\stage_ -> miscsStep pos obj stage_ interfered)
  in
    stage.map
      |> Dict.toList
      |> List.foldl acc (Random.constant stage)

accBounded : Map -> ( Coord, Object ) -> List Coord -> List Coord
accBounded map ( pos, obj ) bounded =
  case obj of
    Magnet axis ->
      let
        targets =
          axis
            |> Axis.toDirections
            |> List.filterMap (\d ->
              let
                p = pos |> towards d
              in
                map
                  |> Dict.get p
                  |> Maybe.map (\o -> ( p, o )))
            |> List.filter (\( _, o ) -> Object.isFerromagnet axis o)
            |> List.map Tuple.first
      in
        targets ++ bounded

    _ ->
      bounded

magnetsStep : ( Coord, Object ) -> ( Map, List Coord, List Coord ) -> ( Map, List Coord, List Coord )
magnetsStep ( pos, obj ) prev =
  let
    ( _, _, canceled ) = prev
  in
    if canceled |> List.member pos then
      prev
    else
      case obj of
        Magnet axis ->
          axis
            |> Axis.toDirections
            |> List.foldl (tryPull pos axis) prev
        _ -> prev

tryPull pos axis d ( map, bounded, canceled ) =
  let
    front = pos |> towards d

    back = pos |> towards d |> towards d

    backObj = map |> Dict.get back

    spaceInFront =
      map
        |> Dict.get front
        |> (==) Nothing

    ferromagneticInBack =
      backObj
        |> Maybe.map (Object.isFerromagnet axis)
        |> Maybe.withDefault False

    notBounded =
      not ((bounded |> List.member back) || (canceled |> List.member back))
  in
    if spaceInFront && ferromagneticInBack && notBounded then
      let
        map_ =
          map
            |> Dict.remove back
            |> Dict.update front (\_ -> backObj)

        bounded_ =
          front :: bounded

        canceled_ =
          back :: canceled
      in
        ( map_, bounded_, canceled_ )
    else
      ( map, bounded, canceled )


pushersStep : ( Coord, Object ) -> ( Map, List Coord ) -> ( Map, List Coord )
pushersStep ( pos, obj ) prev =
  let
    ( map, interfered ) = prev
  in
    if interfered |> List.member pos then
      prev
    else
      case obj of
        Pusher d 0 ->
          let
            p1 = towards d pos
            p2 = towards d p1
            o1 = Dict.get p1 map
            entryType =
              o1
                |> Maybe.map
                  ( \o -> case Object.reaction o of
                    Movable ->
                      if (map |> Dict.member p2) || (interfered |> List.member p1) then
                        CannotEntry
                      else
                        case o of
                          Pusher _ _ -> CannotEntry
                          _          -> PushEntry o
                    Fixed -> CannotEntry
                    _ -> CannotEntry
                  )
                |> Maybe.withDefault JustEntry
          in
            case entryType of
              JustEntry ->
                let
                  map_ =
                    map
                      |> Dict.remove pos
                      |> Dict.insert p1 (Pusher d pusherWait)
                in
                  ( map_, interfered )
              PushEntry o ->
                let
                  map_ =
                    map
                      |> Dict.remove p1
                      |> Dict.insert p2 o
                      |> Dict.insert pos (Pusher (d |> Direction.mirror) pusherWait)
                in
                  ( map_, p2 :: interfered )
              CannotEntry ->
                let
                  map_ =
                    map
                      |> Dict.insert pos (Pusher (d |> Direction.mirror) pusherWait)
                in
                  ( map_, interfered )
              _ -> prev

        Pusher d n ->
          let
            nextPusher =
              if interfered |> List.member pos then
                Pusher d pusherWait
              else
                Pusher d (n-1)

            map_ =
              map
                |> Dict.insert pos nextPusher
          in
            ( map_, interfered )

        _ -> prev

miscsStep : Coord -> Object -> Stage -> List Coord -> Random.Generator Stage
miscsStep pos obj stage interfered =
  let
    bounded =
      interfered |> List.member pos
  in
    case obj of
      Kiki direction ->
        if bounded then
          stage |> Random.constant
        else
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
                  if bounded then
                    pos
                  else
                    direction
                      |> Maybe.map (\d -> pos |> towards d)
                      |> Maybe.withDefault pos

                removedMap =
                  stage.map |> Dict.remove pos

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
      _ -> stage |> Random.constant

pakuPos : Stage -> Coord
pakuPos stage =
  stage.map
    |> Dict.toList
    |> List.filter (\( _, obj ) -> obj == Paku)
    |> List.head
    |> Maybe.map Tuple.first
    |> Maybe.withDefault ( -1, -1 )

pusherWait = 6

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
        then obj |> Object.toSvg x y |> fadeOut
        else obj |> Object.toSvg x y)
    )

fadeOut content =
  Svg.g
    [ class "fade-out" ]
    [ content ]



-- SERIALIZE --

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
          |> Object.toChar)
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
              Object.fromChar char
                |> Maybe.map(\o -> (coord, o))))
        |> List.concat
        |> Dict.fromList
  in
  fromDict map



-- UTILITIES --

prepend : Maybe a -> List a -> List a
prepend maybe list =
  case maybe of
    Nothing -> list
    Just a  -> a :: list