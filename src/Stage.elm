module Stage exposing
  ( Stage
  , empty
  , isCleared
  , move
  , enemyTurn
  , view
  , toString
  , fromString
  )

import Direction exposing (Direction(..))
import Mapchip exposing (Mapchip(..), Movility(..))

import Dict exposing (Dict)
import Random exposing (Seed)
import Html exposing (Html)
import Svg exposing (svg)



type alias Stage =
  { map: Dict Coords Mapchip
  , playerPos: Coords
  , gems: Int
  }

type alias Coords = (Int, Int)

empty =
  { map = Dict.empty
  , playerPos = (0, 0)
  , gems = 0
  }


isCleared: Stage -> Bool
isCleared stage =
  stage.gems == 0

type EntryType
 = JustEntry
 | PushEntry Mapchip
 | TakeEntry Mapchip
 | CannotEntry

move: Direction -> Stage -> Stage
move direction {map, playerPos, gems} =
  let
    p1 = towards direction playerPos
    p2 = towards direction p1
    o1 = Dict.get p1 map
    entryType =
      o1
        |> Maybe.map
          ( \o -> case Mapchip.movility o of
            Takable -> TakeEntry o
            Movable ->
              if map |> Dict.member p2
              then CannotEntry
              else PushEntry o
            Fixed -> CannotEntry
          )
        |> Maybe.withDefault JustEntry
    takeGem =
      o1
        |> Maybe.map (\o -> o == Gem)

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
    newPlayerPos =
      case entryType of
        JustEntry   -> p1
        PushEntry _ -> p1
        TakeEntry _ -> p1
        CannotEntry -> playerPos
    newGems =
      if o1 == Just Gem
      then gems - 1
      else gems

  in
    {map = newMap, playerPos = newPlayerPos, gems = newGems}

-- 上書きして移動
moveObject: Coords -> Direction -> Dict Coords Mapchip -> Dict Coords Mapchip
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
    enemyAction: Coords -> Mapchip -> (Dict Coords Mapchip, Seed) -> (Dict Coords Mapchip, Seed)
    enemyAction pos obj (map, seed_) =
      case obj of
        Kiki direction ->
          case map |> Dict.get (pos |> towards direction) of
            Nothing ->
              ( map |> moveObject pos direction
              , seed_ )
            Just ClockwiseBlock ->
              ( map |> Dict.insert pos (Kiki (Direction.rotateClockwise direction))
              , seed_ )
            Just AntiClockwiseBlock ->
              ( map |> Dict.insert pos (Kiki (Direction.rotateAntiClockwise direction))
              , seed_ )
            _ -> (map, seed_)
        _ -> (map, seed_)

    (newMap, seed__) =
      stage.map
        |> Dict.toList
        |> List.foldr (\(p,o) -> enemyAction p o) (stage.map, seed)
  in
    { stage | map = newMap }


view: Stage -> Html msg
view stage =
  let (px, py) = stage.playerPos in
  Svg.svg[]
    ( stage.map
      |> Dict.toList
      |> List.concatMap (\((x, y), mapchip) -> Mapchip.toSvg x y mapchip)
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
    toMapchip c =
      case c of
        "W" -> Just Wall
        "G" -> Just Gem
        "B" -> Just Block
        "8" -> Just (Kiki Up)
        "2" -> Just (Kiki Down)
        "4" -> Just (Kiki Left)
        "6" -> Just (Kiki Right)
        "," -> Just ClockwiseBlock
        ";" -> Just AntiClockwiseBlock
        "C" -> Just CrackedBlock
        _ -> Nothing
    map =
      src
        |> String.split "\n"
        |> List.map (\s -> s |> String.split "")
        |> List.indexedMap (\y -> \l -> l
          |> List.indexedMap (\x -> \c ->
            (toMapchip c |> Maybe.map(\m -> ((x, y), m))))
          |> List.filterMap (\m -> m))
        |> List.concatMap (\l -> l)
        |> Dict.fromList
    gems =
      map
        |> Dict.values
        |> List.filter (\o -> o == Gem)
        |> List.length
  in
    {map = map |> Dict.insert (1,1) Paku, playerPos = (1,1), gems = gems}
