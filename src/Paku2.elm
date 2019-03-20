module Paku2 exposing (init)

import Direction exposing (Direction(..))
import Stage exposing (Stage, GameState(..))
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
  , frame : Int
  , stageSrc : String
  , stageEncoded : String
  }

type InputState
 = WaitForPalyerInput
 | WaitForEnemyTurn
 | ForceEnemyTurn Direction
 | WaitForAnimation

type alias Flags =
  { stage : String }

init : Flags -> (Model, Cmd Msg)
init flags = ( initModel flags, encodeUri flags.stage )
initModel flags =
  { stage = Stage.fromString flags.stage
  , inputState = WaitForPalyerInput
  , frame = 0
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
  | ForceTick Direction
  | EnemyTurn Int
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
        if Stage.gameState model.stage /= Playing
        then
          ( model, Cmd.none )
        else
          ( model, requestEnemyTurn )

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
        ( { model | inputState = WaitForPalyerInput }
        , Cmd.none
        )

      UriEncoded str ->
        ( { model | stageEncoded = str }
        , Cmd.none
        )

requestEnemyTurn =
  Random.generate EnemyTurn (Random.int Random.minInt Random.maxInt)

enemyTurn seed model =
  let nextStage = Stage.enemyTurn seed model.stage in
  { model
  | inputState =
      if (nextStage |> Stage.gameState) == GameOver
      then WaitForAnimation
      else WaitForPalyerInput
  , stage = nextStage
  , frame = model.frame + 1
  }



-- VIEW --

view : Model -> Html Msg
view model =
  let
    top =
      if model.inputState == WaitForAnimation
      then
        [ Stage.view model.stage
        , buttons
        ]
      else case model.stage |> Stage.gameState of
        Playing ->
          [ Stage.view model.stage
          , buttons
          ]
        Clear ->
          [ Html.p[][text "くりあ～"]
          , Html.input[Attr.type_ "button", onClick LoadStage, Attr.value "リセット"][]
          ]
        GameOver ->
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

    ForceEnemyTurn direction ->
      Time.every 50 (\_ -> ForceTick direction)

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

