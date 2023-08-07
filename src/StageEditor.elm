module StageEditor exposing (init)

import Direction exposing (Direction(..))
import Stage exposing (Stage)
import Object exposing (Object(..))
import Ports exposing (encodeUri, onUriEncoded)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (Svg)
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
  , palletIndex : (Int, Int)
  }

type alias Flags =
  { stage : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { stageEncoded = ""
    , stage = Stage.fromString flags.stage
    , palletIndex = (0, 0)
    }
  , encodeUri flags.stage
  )



-- UPDATE --

type Msg
  = PalletClicked Int Int
  | StageClicked Int Int
  | OnUriEncoded String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PalletClicked x y ->
      ( { model | palletIndex = (x, y) }, Cmd.none )

    StageClicked x y ->
      let
        (px, py) =
          model.palletIndex

        maybeObj =
          palletList
            |> at py
            |> Maybe.andThen (at px)
            |> Maybe.andThen (\o -> o)

        newStage =
          model.stage
           |> Stage.put x y maybeObj
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

stageTouchArea : Int -> Int -> Svg Msg
stageTouchArea w h =
  List.range 0 (w-1)
    |> List.concatMap (\x -> List.range 0 (h-1)
      |> List.map (\y ->
        Object.touchArea x y (StageClicked x y)))
    |> Svg.g []

editableStageView stage =
  Svg.g []
    [ stage |> Stage.view
    , stageTouchArea 12 10
    ]
    |> List.singleton |> Svg.svg []
    |> List.singleton |> Html.div []

palletList : List (List (Maybe Object))
palletList =
  [ [ Just Paku
    , Just Wall
    , Just (Gem Up 0)
    , Just Block
    , Just (Kiki Up)
    , Just ClockwiseBlock
    , Just (Spinner 0)
    , Just (Pusher Up 0)
    ]
  , [ Nothing
    , Nothing
    , Nothing
    , Just CrackedBlock
    , Just (Kiki Right)
    , Just AntiClockwiseBlock
    , Nothing
    , Just (Pusher Right 0)
    ]
  , [ Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just (Kiki Down)
    , Nothing
    , Nothing
    , Just (Pusher Down 0)
    ]
  , [ Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just (Kiki Left)
    , Nothing
    , Nothing
    , Just (Pusher Left 0)
    ]
  ]

palletView : (Int, Int) -> Html Msg
palletView index =
  palletList
    |> List.indexedMap (\y ls -> ls
      |> List.indexedMap (\x o ->
        let
          maybeCursor =
            if (x, y) == index
            then Just (cursor index)
            else Nothing

          justTouchArea =
            Just (Object.touchArea x y (PalletClicked x y))

          maybeObj =
            o |> Maybe.map (Object.toSvg x y)
        in
          []
            |> prepend maybeCursor
            |> prepend justTouchArea
            |> prepend maybeObj
            |> Svg.g []
      )
    )
    |> List.concat
    |> Svg.svg []

cursor (x, y) =
  Svg.rect
    [ Svg.Attributes.x <| String.fromInt <| (Object.chipSize * x)
    , Svg.Attributes.y <| String.fromInt <| (Object.chipSize * y)
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



-- UTILITIES --

prepend : Maybe a -> List a -> List a
prepend maybe list =
  case maybe of
    Nothing -> list
    Just a  -> a :: list

at : Int -> List a -> Maybe a
at index list =
  case list of
    [] ->
      Nothing

    hd :: tl ->
      if index == 0 then
        Just hd
      else at (index - 1) tl
