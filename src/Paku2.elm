module Paku2 exposing (init)

import Direction exposing (Direction(..))
import Stage exposing (Stage, State(..))
import Ports exposing (encodeUri, onUriEncoded, decodeUri, onUriDecoded)

import Dict exposing (Dict)
import Random
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Task exposing (Task)
import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch exposing (onStart)
import Svg
import Svg.Attributes
import Svg.Events exposing (onMouseDown)

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
  , stageSrc : String
  , stageEncoded : String
  }

type InputState
  = WaitForPlayerInput
  | WaitForEnemyTurn
  | ForceEnemyTurn -- タイマーリセットに必要
  | WaitForAnimation

type alias Flags =
  { stage : String }

init : Flags -> (Model, Cmd Msg)
init flags = ( initModel flags, encodeUri flags.stage )
initModel flags =
  { stage = Stage.fromString flags.stage
  , inputState = WaitForPlayerInput
  , stageSrc = flags.stage
  , stageEncoded = ""
    --"WWWWWWWWWW\n"++
    --"W    ,  +W\n"++
    --"W   , C2,W\n"++
    --"W     ;  W\n"++
    --"WG ,     W\n"++
    --"W < B >,GW\n"++
    --"WWWWWWWWWW"
  }



-- UPDATE --

type Msg
  = Nop
  | Tick
  | Key Direction
  | EnemyTurn Stage
  | StageSrcChanged String
  | LoadStage
  | AnimationFinished
  | UriEncoded String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      Nop ->
        ( model, Cmd.none )

      Tick ->
        if Stage.state model.stage /= Playing then
          ( model, Cmd.none )
        else
          ( model, requestEnemyTurn model.stage )

      Key direction ->
        if Stage.state model.stage /= Playing then
          ( model, Cmd.none )
        else if model.inputState == WaitForEnemyTurn then
          forceTick model direction
        else
          let
            nextStage = Stage.move direction model.stage
            newModel =
              { model
              | stage = nextStage
              , inputState =
                if Stage.state model.stage == Miss then
                  WaitForAnimation
                else
                  WaitForEnemyTurn
              }
          in
            ( newModel, Cmd.none )

      EnemyTurn stage ->
        ( { model
          | stage = stage
          , inputState =
              if Stage.state stage == Miss then
                WaitForAnimation
              else
                WaitForPlayerInput
          }
        , Cmd.none
        )

      StageSrcChanged src ->
        ( { model | stageSrc = src }
        , Cmd.none
        )

      LoadStage ->
        ( { model | stage = Stage.fromString model.stageSrc }
        , Cmd.none
        )

      AnimationFinished ->
        ( { model | inputState = WaitForPlayerInput }
        , Cmd.none
        )

      UriEncoded str ->
        ( { model | stageEncoded = str }
        , Cmd.none
        )

requestEnemyTurn stage =
  Stage.enemyTurn stage
    |> Random.generate EnemyTurn


forceTick model direction =
  ( { model | inputState = ForceEnemyTurn }
  , Cmd.batch
    [ requestEnemyTurn model.stage
    , Task.perform Key (Task.succeed direction)
    ]
  )



-- VIEW --

view : Model -> Html Msg
view model =
  let
    top =
      if model.inputState == WaitForAnimation then
        [ Stage.view model.stage
        , buttons
        ]
      else
        case model.stage |> Stage.state of
          Playing ->
            [ Stage.view model.stage
            , buttons
            ]
          Cleared ->
            [ Html.p[][text "くりあ～"]
            , Html.input[Attr.type_ "button", onClick LoadStage, Attr.value "リセット"][]
            ]
          Miss ->
            [ Html.p[][text "ミス"]
            , Html.input[Attr.type_ "button", onClick LoadStage, Attr.value "リセット"][]
            ]
  in
    List.concat
      [ top
      , [ Html.p[]
          [ Html.a
            [ Attr.href ("editor.html?stage="++model.stageEncoded) ]
            [ Html.text "edit" ]
          ]
        ]
      ]
      |> Html.div []

buttons =
  Svg.svg []
    [ button  70  35 0 (Key Up)
    , button 105  70 1 (Key Right)
    , button  70 105 2 (Key Down)
    , button  35  70 3 (Key Left)
    ]

button x y r key =
  Svg.g
    [ Svg.Attributes.transform
        <| "translate("++ String.fromInt x ++","++ String.fromInt y ++")"
        ++ "rotate("++ String.fromInt (r*90) ++") "
    ]
    [ Svg.polygon
        [ Svg.Attributes.points "0,-30 30,0 0,30 -30,0"
        , Svg.Attributes.stroke "#AAAAAA"
        , Svg.Attributes.fill   "#DDDDDD"
        , onMouseDown (key)
        , onStart (\_ -> key)
        ]
        []
    , Svg.polygon
        [ Svg.Attributes.points "0,-10 10,0 -10,0"
        , Svg.Attributes.fill   "#000000"
        ][]
    ]

onTouch direction =
  [ onMouseDown (Key direction)
  , onStart (\e -> Key direction)
  ]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.inputState of

    ForceEnemyTurn ->
      Time.every 100 (\_ -> Tick)

    WaitForAnimation ->
      Time.every 500 (\_ -> AnimationFinished)

    _ ->
      Sub.batch
        [ onKeyDown keyDecoder
        , Time.every 150 (\_ -> Tick)
        , onUriEncoded UriEncoded
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

