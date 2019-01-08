module Stage exposing
  ( Stage
  , testStage
  , isCleared
  , move
  , enemyTurn
  , view
  , toString
  )

import Dict exposing (Dict)
import Html exposing (Html)
import Svg exposing (svg)

import Direction exposing (Direction(..))
import Mapchip exposing (Mapchip(..))

type alias Stage =
  { map: Dict Coords Mapchip
  , playerPos: Coords
  , gems: Int
  }

type alias Coords = (Int, Int)

testStage: Stage
testStage =
  let
    src =
      "WWWWW\n"++
      "W   W\n"++
      "WG GW\n"++
      "WWWWW"
  in
    fromString src


isCleared: Stage -> Bool
isCleared stage =
  stage.gems == 0

move: Direction -> Stage -> Stage
move direction stage =
  let
    (x, y) = stage.playerPos

    newPos =
      case direction of
        Up ->    (x    , y - 1)
        Down ->  (x    , y + 1)
        Left ->  (x - 1, y    )
        Right -> (x + 1, y    )

    noEntry =
      stage.map
        |> Dict.get newPos
        |> Maybe.map Mapchip.noEntry
        |> Maybe.withDefault False

    getGem =
      stage.map
        |> Dict.get newPos
        |> Maybe.map (\m -> m == Gem)
        |> Maybe.withDefault False

    gems =
      if getGem
      then stage.gems - 1
      else stage.gems

    map =
      if getGem
      then stage.map |> Dict.remove newPos
      else stage.map
  in
    if noEntry
    then stage
    else { map = map, playerPos = newPos, gems = gems }

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
