module Stage exposing
  ( Stage
  , empty
  , GameState(..)
  , gameState
  , move
  , enemyTurn
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



type alias Stage =
  { map: Dict Coords Object
  , playerPos: Coords
  , miss: Bool
  , gems: Int
  }

type alias Coords = (Int, Int)

empty =
  { map = Dict.empty
  , playerPos = (0, 0)
  , miss = False
  , gems = 0
  }

type GameState
  = Playing
  | Clear
  | GameOver

gameState: Stage -> GameState
gameState stage =
  if stage.gems == 0
  then Clear
  else if stage.miss
  then GameOver
  else Playing

type EntryType
 = JustEntry
 | PushEntry Object
 | TakeEntry Object
 | CannotEntry
 | Damaged

move: Direction -> Stage -> Stage
move direction {map, playerPos, miss, gems} =
  let
    p1 = towards direction playerPos
    p2 = towards direction p1
    o1 = Dict.get p1 map
    entryType =
      o1
        |> Maybe.map
          ( \o -> case Object.reaction o of
            Takable -> TakeEntry o
            Movable ->
              if map |> Dict.member p2
              then CannotEntry
              else PushEntry o
            Fixed -> CannotEntry
            Aggressive -> Damaged
          )
        |> Maybe.withDefault JustEntry

    newMap =
      case entryType of
        JustEntry ->
          map |> moveObject playerPos direction
        PushEntry o ->
          map
            |> moveObject p1 direction
            |> moveObject playerPos direction
        TakeEntry o ->
          map |> moveObject playerPos direction
        CannotEntry -> map
        Damaged -> map
    newPlayerPos =
      case entryType of
        JustEntry   -> p1
        PushEntry _ -> p1
        TakeEntry _ -> p1
        CannotEntry -> playerPos
        Damaged -> playerPos
    newMiss =
      case entryType of
        Damaged -> True
        _ -> False
    newGems =
      case o1 of
        Just (Gem _ _) -> gems - 1
        _ -> gems

  in
    { map = newMap
    , playerPos = newPlayerPos
    , miss = newMiss
    , gems = newGems}

-- 上書きして移動
moveObject: Coords -> Direction -> Dict Coords Object -> Dict Coords Object
moveObject pos direction map =
  let
    newPos = pos |> towards direction
  in
    map
      |> Dict.get pos
      |> Maybe.map (\obj ->
        map
          |> Dict.remove pos
          |> Dict.insert newPos obj)
      |> Maybe.withDefault map

towards direction (x, y) =
  case direction of
        Up ->    (x    , y - 1)
        Down ->  (x    , y + 1)
        Left ->  (x - 1, y    )
        Right -> (x + 1, y    )

enemyTurn: Seed -> Stage -> Stage
enemyTurn seed stage =
  let
    (newMap, newMiss, _) =
      stage.map
        |> Dict.toList
        |> List.foldr (\(p,o) -> enemyAction p o stage.playerPos) (stage.map, stage.miss, seed)
  in
    { stage | map = newMap, miss = newMiss }

enemyAction: Coords -> Object -> Coords -> (Dict Coords Object, Bool, Seed) -> (Dict Coords Object, Bool, Seed)
enemyAction pos obj playerPos ( map, damaged, seed ) =
  case obj of
    Kiki direction ->
      let
        map_ =
          case map |> Dict.get (pos |> towards direction) of
            Nothing ->
              map |> moveObject pos direction
            Just ClockwiseBlock ->
              map |> Dict.insert pos (Kiki (Direction.rotateClockwise direction))
            Just AntiClockwiseBlock ->
              map |> Dict.insert pos (Kiki (Direction.rotateAntiClockwise direction))
            _ -> map
      in
        ( map_, damaged, seed )

    Gem frame 0 ->
      let
        ((nextFrame, nextRemaining), nextSeed) =
          Random.step gemGenerator seed
      in
        ( (map |> Dict.insert pos ( Gem nextFrame nextRemaining )), damaged, nextSeed )
    Gem frame remaining ->
      ( (map |> Dict.insert pos (Gem frame (remaining - 1))), damaged, seed )

    Spinner i ->
      let
        nextFrame =
          case i of
            2 -> 0
            n -> n + 1
        nextSpinner = Spinner nextFrame

        (md, nextSeed) =
          spinnerAi pos playerPos seed
        nextPos =
          md
            |> Maybe.map (\d -> pos |> towards d)
            |> Maybe.withDefault pos

        removedMap = map |> Dict.remove pos
        (nextMap, nextDamaged) =
            case removedMap |> Dict.get nextPos of
              Nothing -> (removedMap |> Dict.insert nextPos nextSpinner, damaged)
              Just Paku -> (removedMap |> Dict.insert pos nextSpinner, True)
              _ -> (removedMap |> Dict.insert pos nextSpinner, damaged)
      in
        ( nextMap, nextDamaged, nextSeed )

    _ -> ( map, damaged, seed )

gemGenerator: Random.Generator (Direction, Int)
gemGenerator =
  Random.pair randomDirecction (Random.int 5 10)

spinnerAi (sx, sy) (px, py) seed =
  let
    (i, seed1) = Random.step (Random.int 0 3) seed
    (d, seed2) =
      case i of
        0 -> (Just (pursuit sx sy px py), seed1)
        1 -> Random.step randomDirecction seed1 |> (\(d_, s_) -> (Just d_, s_))
        _ -> (Nothing, seed1)
  in
    (d, seed2)

pursuit ex ey px py =
  if (px - ex)*(px - ex) > (py - ey)*(py - ey)
  then if px - ex > 0
    then Right
    else Left
  else if py - ey > 0
    then Down
    else Up

randomDirecction: Random.Generator Direction
randomDirecction =
  Random.int 0 3
    |> Random.map (\n ->
      case n of
        0 -> Up
        1 -> Down
        2 -> Left
        _ -> Right)

view: Stage -> Html msg
view stage =
  let (px, py) = stage.playerPos in
  Svg.svg[]
    ( stage.map
      |> Dict.toList
      |> List.map (\((x, y), obj) -> obj |> Object.toSvg x y)
    )

toString: Stage -> String
toString stage =
  "Stage { player: " ++
  coordsToString stage.playerPos ++
  ", gems: " ++
  String.fromInt stage.gems ++
  "}"

coordsToString (x, y) =
  "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

fromString: String -> Stage
fromString src =
  let
    toObject c =
      case c of
        "W" -> Just Wall
        "G" -> Just (Gem Up 0)
        "B" -> Just Block
        "8" -> Just (Kiki Up)
        "2" -> Just (Kiki Down)
        "4" -> Just (Kiki Left)
        "6" -> Just (Kiki Right)
        "," -> Just ClockwiseBlock
        ";" -> Just AntiClockwiseBlock
        "C" -> Just CrackedBlock
        "+" -> Just (Spinner 0)
        _ -> Nothing
    map =
      src
        |> String.split "\n"
        |> List.map (\s -> s |> String.split "")
        |> List.indexedMap (\y -> \l -> l
          |> List.indexedMap (\x -> \c ->
            (toObject c |> Maybe.map(\m -> ((x, y), m))))
          |> List.filterMap (\m -> m))
        |> List.concatMap (\l -> l)
        |> Dict.fromList
    gems =
      map
        |> Dict.values
        |> List.filter (\o -> case o of
          Gem _ _ -> True
          _ -> False)
        |> List.length
  in
    { map = map |> Dict.insert (1,1) Paku
    , playerPos = (1,1)
    , miss = False
    , gems = gems}
