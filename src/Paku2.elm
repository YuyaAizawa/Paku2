module Paku2 exposing (init)

import Direction exposing (Direction(..))
import Stage exposing (Stage)

import Dict exposing (Dict)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Task exposing (Task)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Svg
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
  }

type InputState
 = WaitForPalyerInput
 | WaitForEnemyTurn
 | ForceEnemyTurn Direction

init : () -> (Model, Cmd Msg)
init _ = ( initModel, Task.perform (\_ -> LoadStage) (Task.succeed ()))
initModel =
  { stage = Stage.empty
  , inputState = WaitForPalyerInput
  , frame = 0
  , stageSrc =
    "WWWWWWWWW\n"++
    "W   ,   W\n"++
    "W  , C2,W\n"++
    "W    ;  W\n"++
    "WG,     W\n"++
    "W B   ,GW\n"++
    "WWWWWWWWW"
  }

modelToString {stage, frame, inputState} =
  "stage: " ++ Stage.toString stage ++ ", frame: " ++ String.fromInt frame ++ ", inputState: " ++ inputStateToString inputState

inputStateToString inputState =
  case inputState of
    WaitForPalyerInput -> "WaitForPalyerInput"
    WaitForEnemyTurn -> "WaitForEnemyTurn"
    ForceEnemyTurn direction -> "ForceEnemyTurn"



-- UPDATE --

type Msg
  = Nop
  | Tick
  | Key Direction
  | ForceTick Direction
  | StageSrcChanged String
  | LoadStage

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      Nop ->
        ( model, Cmd.none )

      Tick ->
        ( enemyTurn model, Cmd.none )

      Key direction ->
        let
          newMoldel =
            if model.inputState == WaitForEnemyTurn
            then { model | inputState = ForceEnemyTurn direction }
            else { model
              | stage = model.stage
                |> Stage.move direction
              , inputState = WaitForEnemyTurn }
        in
          ( newMoldel, Cmd.none )

      ForceTick direction ->
        (enemyTurn model, Task.perform Key (Task.succeed direction))

      StageSrcChanged src ->
        ( { model | stageSrc = src }, Cmd.none )

      LoadStage ->
        ( { model | stage = Stage.fromString model.stageSrc }, Cmd.none )

enemyTurn {inputState, frame, stage, stageSrc} =
  { inputState = WaitForPalyerInput
  , frame = frame + 1
  , stage = Stage.enemyTurn stage
  , stageSrc = stageSrc
  }





-- VIEW --

view : Model -> Html Msg
view model =
  if Stage.isCleared model.stage
  then
    Html.div[]
     [ Html.p[][text "くりあ～"]
     , Html.input[Attr.type_ "button", onClick LoadStage, Attr.value "リセット"][]
     ]
  else
    Html.div[]
      [ Html.p[][text (modelToString model)]
      , Stage.view model.stage
      , buttons
      , stageEditor model.stageSrc]

buttons =
  Html.table[]
    [ Html.tr[]
      [ Html.td[][]
      , Html.td[][Html.button [onClick (Key Up)][text "↑"]]
      , Html.td[][]
      ]
    , Html.tr[]
      [ Html.td[][Html.button [onClick (Key Left)][text "←"]]
      , Html.td[][Html.button [onClick (Key Down)][text "↓"]]
      , Html.td[][Html.button [onClick (Key Right)][text "→"]]
      ]
    ]

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

