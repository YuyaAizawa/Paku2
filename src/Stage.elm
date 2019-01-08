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
  , remainingJems: Int
  }

type alias Coords = (Int, Int)

testStage: Stage
testStage =
  let
    src =
      "WWWWW\n"++
      "W   W\n"++
      "W  JW\n"++
      "WWWWW"
  in
    fromString src


isCleared: Stage -> Bool
isCleared stage =
  stage.remainingJems == 0

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

    getJem =
      stage.map
        |> Dict.get newPos
        |> Maybe.map (\m -> m == Jem)
        |> Maybe.withDefault False

    jems =
      if getJem
      then stage.remainingJems - 1
      else stage.remainingJems
  in
    if noEntry
    then stage
    else { stage
      | playerPos = newPos
      , remainingJems = jems
      }

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
  "Stage { player: " ++ coordsToString stage.playerPos ++ " }"

coordsToString (x, y) =
  "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

fromString: String -> Stage
fromString src =
  let
    toMapchip c =
      case c of
        "W" -> Just Wall
        "J" -> Just Jem
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
    jems =
      map
        |> Dict.values
        |> List.filter (\o -> o == Jem)
        |> List.length
  in
    {map = map, playerPos = (1,1), remainingJems = jems}
