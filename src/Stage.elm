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

import Dict exposing (Dict)
import Html exposing (Html)
import Svg exposing (svg)

import Direction exposing (Direction(..))
import Mapchip exposing (Mapchip(..), Movility(..))

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
    p1 = getCoords direction playerPos
    p2 = getCoords direction p1
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
        JustEntry -> map
        PushEntry o ->
          map
            |> Dict.insert p2 o
            |> Dict.remove p1
        TakeEntry o ->
          map
            |> Dict.remove p1
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

getCoords direction (x, y) =
  case direction of
        Up ->    (x    , y - 1)
        Down ->  (x    , y + 1)
        Left ->  (x - 1, y    )
        Right -> (x + 1, y    )

enemyTurn: Stage -> Stage
enemyTurn stage = stage -- stab

view: Stage -> Html msg
view stage =
  let (px, py) = stage.playerPos in
  Svg.svg[]
    (( stage.map
      |> Dict.toList
      |> List.concatMap (\((x, y), mapchip) -> Mapchip.toSvg x y mapchip)
    ) ++ [Mapchip.playerSvg px py])

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
    {map = map, playerPos = (1,1), gems = gems}
