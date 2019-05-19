module Stage exposing
  ( Stage
  , empty
  , get
  , put
  , GameState(..)
  , gameState
  , move
  , AiStrategyContext
  , enemyTurnStart
  , reflectOn
  , nextStep
  , view
  , toString
  , export
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
  { map : Dict Coords Object
  , width : Int
  , height : Int
  , miss : Bool
  , gems : Int
  }

type alias Coords = (Int, Int)

empty =
  { map = Dict.empty
  , width = 2
  , height = 2
  , playerPos = (0, 0)
  , miss = False
  , gems = 0
  }

get : Int -> Int -> Stage -> Maybe Object
get x y stage =
  stage.map
    |> Dict.get (x, y)

put : Int -> Int -> Maybe Object -> Stage -> Stage
put x y obj stage =
  if x < 1 || stage.width < (x-1)
    || y < 1 || stage.height < (y-1)
  then stage
  else if get x y stage == Just Paku
  then stage
  else
    let
      map = case obj of
        Just obj_ -> stage.map |> Dict.insert (x, y) obj_
        Nothing -> stage.map |> Dict.remove (x, y)
    in
    fromDict map

fromDict : Dict Coords Object -> Stage
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
  , width = w
  , height = h
  , miss = False
  , gems = gems
  }

type GameState
  = Playing
  | Clear
  | GameOver

gameState : Stage -> GameState
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



move : Direction -> Stage -> Stage
move direction {map, width, height, miss, gems} =
  let
    playerPos = getPlayerPos map
    p1 = towards direction playerPos
    p2 = towards direction p1
    o1 = Dict.get p1 map
    entryType =
      case o1 of
        Nothing -> JustEntry
        Just obj -> case obj |> Object.reaction of
          Takable -> TakeEntry obj
          Movable ->
            if map |> Dict.member p2
            then CannotEntry
            else PushEntry obj
          Fixed -> CannotEntry
          Aggressive -> Damaged

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
  , width = width
  , height = height
  , miss = newMiss
  , gems = newGems
  }

-- 上書きして移動
moveObject : Coords -> Direction -> Dict Coords Object -> Dict Coords Object
moveObject pos direction map =
  let
    newPos = pos |> towards direction
  in
    case map |> Dict.get pos of
      Nothing -> map
      Just obj ->
        map
          |> Dict.remove pos
          |> Dict.insert newPos obj

towards direction (x, y) =
  case direction of
        Up ->    (x    , y - 1)
        Down ->  (x    , y + 1)
        Left ->  (x - 1, y    )
        Right -> (x + 1, y    )

enemyTurnStart : Stage -> Maybe (Random.Generator AiStrategyContext)
enemyTurnStart stage =
  nextStep stage
    ( ( stage.map, False )
    , stage.map |> Dict.toList
    )

type alias AiStrategyContext = (AiStrategyResult, AiStrategyTask)
type alias AiStrategyTask = List (Coords, Object)
type alias AiStrategyResult = (Dict Coords Object, Bool)

reflectOn : Stage -> AiStrategyContext -> Stage
reflectOn stage ( ( map, damaged ), _ ) =
  { stage
  | map = map
  , miss = if stage.miss then True else damaged
  }

nextStep : Stage -> AiStrategyContext -> Maybe (Random.Generator AiStrategyContext)
nextStep stage ( _ , task ) =
  let 
    playerPos = getPlayerPos stage.map
  in case task of
    [] -> Nothing
    hd::tl ->
      let
        strategy =
          developStrategy hd playerPos stage.map
      in
        strategy
          |> Random.map (\s -> (s, tl))
          |> Just


developStrategy : (Coords, Object) -> Coords -> Dict Coords Object -> Random.Generator AiStrategyResult
developStrategy (pos, obj) playerPos map =
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
        ( map_, False ) |> Random.constant

    Gem frame 0 ->
      gemGenerator
            |> Random.map (\( nextFrame, nextRemaining ) ->
              ( map |> Dict.insert pos ( Gem nextFrame nextRemaining )
              , False
              ))
    Gem frame remaining ->
      ( (map |> Dict.insert pos (Gem frame (remaining - 1)))
      , False
      ) |> Random.constant

    Spinner i ->
      let
        nextFrame =
          case i of
            2 -> 0
            n -> n + 1
        nextSpinner = Spinner nextFrame
      in
        spinnerAi pos playerPos
          |> Random.map (\direction ->
            let
              nextPos =
                direction
                  |> Maybe.map (\d -> pos |> towards d)
                  |> Maybe.withDefault pos
              removedMap = map |> Dict.remove pos
              (nextMap, nextDamaged) =
                case removedMap |> Dict.get nextPos of
                  Nothing -> (removedMap |> Dict.insert nextPos nextSpinner, False)
                  Just Paku -> (removedMap |> Dict.insert pos nextSpinner, True)
                  _ -> (removedMap |> Dict.insert pos nextSpinner, False)
            in
              ( nextMap, nextDamaged ) )

    Pusher d 0 ->
      let
        pusherWait = 6
        p1 = towards d pos
        p2 = towards d p1
        o1 = Dict.get p1 map
        entryType =
          o1
            |> Maybe.map
              ( \o -> case Object.reaction o of
                Movable ->
                  if map |> Dict.member p2
                  then CannotEntry
                  else PushEntry o
                Fixed -> CannotEntry
                _ -> CannotEntry
              )
            |> Maybe.withDefault JustEntry

        newMap =
          case entryType of
            JustEntry ->
              map
                |> Dict.remove pos
                |> Dict.insert p1 (Pusher d pusherWait)
            PushEntry o ->
              map
                |> moveObject p1 d
                |> Dict.insert pos (Pusher (d |> Direction.mirror) pusherWait)
            CannotEntry ->
              map
                |> Dict.insert pos (Pusher (d |> Direction.mirror) pusherWait)
            _ -> map
      in
        ( newMap, False ) |> Random.constant

    Pusher d n ->
      let
        newMap = map |> Dict.insert pos (Pusher d (n-1))
      in
        ( newMap, False ) |> Random.constant

    _ -> ( map, False ) |> Random.constant

getPlayerPos map =
  map
    |> Dict.toList
    |> List.filter (\(c, o) -> o == Paku)
    |> List.head
    |> Maybe.map Tuple.first
    |> Maybe.withDefault (0, 0)

gemGenerator : Random.Generator (Direction, Int)
gemGenerator =
  Random.pair randomDirecction (Random.int 5 10)

spinnerAi : Coords -> Coords -> Random.Generator (Maybe Direction)
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

view : Stage -> Html msg
view stage =
  let (px, py) = getPlayerPos stage.map in
  Svg.svg []
    ( stage.map
      |> Dict.toList
      |> List.map (\((x, y), obj) ->
        if obj == Paku && gameState stage == GameOver
        then obj |> Object.toSvg x y |> Object.fadeOut
        else obj |> Object.toSvg x y)
    )

toString : Stage -> String
toString stage =
  "Stage { gems: " ++
  String.fromInt stage.gems ++
  "}"

coordsToString (x, y) =
  "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

export : Stage -> String
export stage =
  List.range 0 (stage.height - 1)
    |> List.concatMap (\y -> List.range 0 stage.width
      |> List.map (\x -> (x, y)))
    |> List.map (\(x, y) ->
      if x == stage.width
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