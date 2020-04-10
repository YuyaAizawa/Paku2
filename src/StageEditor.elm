module StageEditor exposing (init)

import Direction exposing (Direction(..))
import Stage exposing (Stage)
import Object exposing (Object(..))
import Ports exposing (encodeUri, onUriEncoded)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Svg
import Svg.Attributes


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
  , stageEncoded : String
  , palletIndex : Int
  }

type alias Flags =
  { stage : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { stageEncoded = ""
    , stage = Stage.fromString flags.stage
    , palletIndex = 0
    }
  , encodeUri flags.stage
  )


-- UPDATE --
type Msg
  = PalletChanged Int
  | StageClicked Int Int
  | OnUriEncoded String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PalletChanged index ->
      ( { model | palletIndex = index }, Cmd.none )

    StageClicked x y ->
      let
        newStage =
          model.stage
           |> Stage.put x y
              ( palletList
                |> Array.get model.palletIndex
                |> Maybe.andThen (\a -> if a == Nothing then Nothing else a)
              )
      in
      ( { model | stage = newStage }, encodeUri (Stage.toString newStage) )

    OnUriEncoded encoded ->
      ( { model | stageEncoded = encoded }, Cmd.none )



-- VIEW --

view : Model -> Html Msg
view model =
  Html.div []
  [ editableStageView model.stage
  , palletView model.palletIndex
  , Html.a[Attr.href ("index.html?stage="++model.stageEncoded)][Html.text "play"]
  ]

touchArea w h =
  List.range 0 (w-1)
    |> List.concatMap (\x -> List.range 0 (h-1)
      |> List.map (\y ->
        Object.touchArea x y (StageClicked x y)))
    |> Svg.g []

editableStageView stage =
  Svg.g []
    [ stage |> Stage.view
    , touchArea 12 10
    ]
    |> List.singleton |> Svg.svg []
    |> List.singleton |> Html.div []

palletList : Array (Maybe Object)
palletList =
  Nothing ::
  ( [ Paku
    , Wall
    , Gem Up 0
    , Block
    , Kiki Up
    , ClockwiseBlock
    , AntiClockwiseBlock
    , CrackedBlock
    , Spinner 0
    , Pusher Up 0
    ] |> List.map (\o -> Just o)
  ) |> Array.fromList

palletView : Int -> Html Msg
palletView index =
  palletList
    |> Array.toList
    |> List.indexedMap (\i o ->
      let
        svgList = if i == index then [cursor i] else []
        svgList_ = Object.touchArea i 0 (PalletChanged i) :: svgList
        svgList__ = case o of
          Just obj -> Object.toSvg i 0 obj :: svgList_
          Nothing -> svgList_
      in
      Svg.g [] svgList__)
    |> Svg.svg []

cursor i =
  Svg.rect
    [ Svg.Attributes.x <| String.fromInt <| (Object.chipSize * i)
    , Svg.Attributes.y "0"
    , Svg.Attributes.width <| String.fromInt <| Object.chipSize
    , Svg.Attributes.height <| String.fromInt <| Object.chipSize
    , Svg.Attributes.stroke "#FF0000"
    , Svg.Attributes.fill "none"
    ][]

-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onUriEncoded OnUriEncoded
    ]

