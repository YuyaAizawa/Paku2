module Paku2 exposing (init)

import Direction exposing (Direction(..))
import Stage exposing (Stage, GameState(..))

import Dict exposing (Dict)
import Random
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Task exposing (Task)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseUp)
import Html.Events.Extra.Touch exposing (onStart, onEnd, onCancel)
import Svg
import Svg.Attributes
import Time
import Json.Decode as Decode



main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL --

type alias Model =
  { stage : Stage
  , inputState : InputState
  , frame : Int
  , stageSrc : String
  , downButton : Maybe Direction
  }

type InputState
 = WaitForPalyerInput
 | WaitForEnemyTurn
 | ForceEnemyTurn Direction
 | WaitForAnimation

init : () -> (Model, Cmd Msg)
init _ = ( initModel, Task.perform (\_ -> LoadStage) (Task.succeed ()))
initModel =
  { stage = Stage.empty
  , inputState = WaitForPalyerInput
  , frame = 0
  , stageSrc =
    "WWWWWWWWWW\n"++
    "W    ,  +W\n"++
    "W   , C2,W\n"++
    "W     ;  W\n"++
    "WG ,     W\n"++
    "W < B >,GW\n"++
    "WWWWWWWWWW"
  , downButton = Nothing
  }

modelToString {stage, frame, inputState} =
  "stage: " ++ Stage.toString stage ++ ", frame: " ++ String.fromInt frame ++ ", inputState: " ++ inputStateToString inputState

inputStateToString inputState =
  case inputState of
    WaitForPalyerInput -> "WaitForPalyerInput"
    WaitForEnemyTurn -> "WaitForEnemyTurn"
    ForceEnemyTurn direction -> "ForceEnemyTurn"
    WaitForAnimation -> "WaitForAnimation"



-- UPDATE --

type Msg
  = Nop
  | Tick
  | Key Direction
  | ForceTick Direction
  | EnemyTurn Int
  | StageSrcChanged String
  | LoadStage
  | ButtonPressed Direction
  | ButtonReleased
  | AnimationFinished

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      Nop ->
        ( model, Cmd.none )

      Tick ->
        if Stage.gameState model.stage /= Playing
        then
          ( model, Cmd.none )
        else
          case model.downButton of
            Nothing ->
              ( model, requestEnemyTurn )
            Just direction ->
              ( { model | stage = model.stage |> Stage.move direction }
              , requestEnemyTurn
              )

      Key direction ->
        if Stage.gameState model.stage /= Playing
        then
          ( model, Cmd.none )
        else
          let
            newModel =
              if model.inputState == WaitForEnemyTurn
              then { model | inputState = ForceEnemyTurn direction}
              else
                let nextStage = Stage.move direction model.stage in
                { model
                | stage = nextStage
                , inputState =
                  if Stage.gameState nextStage == GameOver
                  then WaitForAnimation
                  else WaitForEnemyTurn }
          in
            ( newModel, Cmd.none )

      ForceTick direction ->
        ( model
        , Cmd.batch
          [ requestEnemyTurn
          , Task.perform Key (Task.succeed direction)
          ]
        )

      EnemyTurn seed ->
        ( enemyTurn (Random.initialSeed seed) model
        , Cmd.none )

      StageSrcChanged src ->
        ( { model | stageSrc = src }
        , Cmd.none )

      LoadStage ->
        ( { model
          | stage = Stage.fromString model.stageSrc
          , downButton = Nothing
          }
        , Cmd.none )

      ButtonPressed direction ->
        ( { model | downButton = Just direction }
        , Task.perform Key (Task.succeed direction))

      ButtonReleased ->
        ( { model | downButton = Nothing }
        , Cmd.none )

      AnimationFinished ->
        ( { model | inputState = WaitForPalyerInput }
        , Cmd.none
        )

requestEnemyTurn =
  Random.generate EnemyTurn (Random.int Random.minInt Random.maxInt)

enemyTurn seed {inputState, frame, stage, stageSrc, downButton} =
  let nextStage = Stage.enemyTurn seed stage in
  { inputState =
      if (nextStage |> Stage.gameState) == GameOver
      then WaitForAnimation
      else WaitForPalyerInput
  , frame = frame + 1
  , stage = nextStage
  , stageSrc = stageSrc
  , downButton = downButton
  }



-- VIEW --

view : Model -> Html Msg
view model =
  if model.inputState == WaitForAnimation
  then playingView model
  else
    case model.stage |> Stage.gameState of
      Playing ->
       playingView model
      Clear ->
        Html.div[]
         [ Html.p[][text "くりあ～"]
         , Html.input[Attr.type_ "button", onClick LoadStage, Attr.value "リセット"][]
         ]
      GameOver ->
        Html.div[]
         [ Html.p[][text "ミス"]
         , Html.input[Attr.type_ "button", onClick LoadStage, Attr.value "リセット"][]
         ]

playingView model =
  Html.div[]
    [ Html.p[][text (modelToString model)]
    , Stage.view model.stage
    , buttons
    , stageEditor model.stageSrc]

buttons =
  Html.table[]
    [ Html.tr[]
      [ Html.td[][]
      , Html.td[][Html.button (onTouch Up)[text "↑"]]
      , Html.td[][]
      ]
    , Html.tr[]
      [ Html.td[][Html.button (onTouch Left)[text "←"]]
      , Html.td[][Html.button (onTouch Down)[text "↓"]]
      , Html.td[][Html.button (onTouch Right)[text "→"]]
      ]
    ]

onTouch direction =
  [ onMouseDown (ButtonPressed direction)
  , onMouseUp (ButtonReleased)
  , onStart (\e -> ButtonPressed direction)
  , onEnd (\e -> ButtonReleased)
  , onCancel (\e -> ButtonReleased)]

stageEditor content =
  Html.div[]
    [ Html.p[][text "すて～じえでぃっと"]
    , Html.textarea
      [ onInput  StageSrcChanged
      , Attr.value content
      , Attr.rows 12
      , Attr.cols 12][]
    , Html.button[onClick LoadStage][text "よみこみ"]
    ]


-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.inputState of

    ForceEnemyTurn direction ->
      Time.every 50 (\_ -> ForceTick direction)

    WaitForAnimation ->
      Time.every 500 (\_ -> AnimationFinished)

    _ ->
      Sub.batch
        [ onKeyDown keyDecoder
        , Time.every 150 (\_ -> Tick)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
  let
    stringToMsg string =
      case string of
        "ArrowUp" -> Key Up
        "ArrowDown" -> Key Down
        "ArrowRight" -> Key Right
        "ArrowLeft" -> Key Left
        _ -> Nop
  in
    Decode.map stringToMsg (Decode.field "key" Decode.string)

