module Paku2 exposing (init)

import Dict exposing (Dict)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Task exposing (Task)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg
import Time
import Json.Decode as Decode

import Direction exposing (Direction(..))
import Stage exposing (Stage)


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
  }

type InputState
 = WaitForPalyerInput
 | WaitForEnemyTurn
 | ForceEnemyTurn Direction

init : () -> (Model, Cmd Msg)
init _ = ( initModel, Cmd.none )
initModel =
  { stage = Stage.testStage
  , inputState = WaitForPalyerInput
  , frame = 0
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
  | Reset

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

      Reset -> init ()

enemyTurn {inputState, frame, stage} =
  { inputState = WaitForPalyerInput
  , frame = frame + 1
  , stage = Stage.enemyTurn stage
  }





-- VIEW --

view : Model -> Html Msg
view model =
  if Stage.isCleared model.stage
  then
    Html.div[]
     [ Html.p[][text "くりあ～"]
     , Html.input[Attr.type_ "button", onClick Reset, Attr.value "リセット"][]
     ]
  else
    Html.div[]
      [ Html.p[][text (modelToString model)]
      , Stage.view model.stage
      ]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.inputState of

    ForceEnemyTurn direction ->
      Time.every 100 (\_ -> ForceTick direction)

    _ ->
      Sub.batch
        [ onKeyDown keyDecoder
        , Time.every 1000 (\_ -> Tick)
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

