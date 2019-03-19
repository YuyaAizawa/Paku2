module Svg.Events.Extra.Touch exposing
  ( Event
  , Keys
  , Touch
  , onStart
  , onMove
  , onEnd
  , onCancel
  )

import Svg
import Svg.Events
import Json.Decode as Decode exposing (Decoder)

type alias Event =
  { keys : Keys
  , changedTouches : List Touch
  , targetTouches : List Touch
  , touches : List Touch
  }

type alias Keys =
  { alt : Bool
  , ctrl : Bool
  , shift : Bool
  }

type alias Touch =
  { identifier : Int
  , clientPos : ( Float, Float )
  , pagePos : ( Float, Float )
  , screenPos : ( Float, Float )
  }

onStart : (Event -> msg) -> Svg.Attribute msg
onStart =
  onWithOptions "touchstart" defaultOptions

onMove : (Event -> msg) -> Svg.Attribute msg
onMove =
  onWithOptions "touchmove" defaultOptions

onEnd : (Event -> msg) -> Svg.Attribute msg
onEnd =
  onWithOptions "touchend" defaultOptions

onCancel : (Event -> msg) -> Svg.Attribute msg
onCancel =
  onWithOptions "touchcancel" defaultOptions

onWithOptions : String -> EventOptions -> (Event -> msg) -> Svg.Attribute msg
onWithOptions event options tag =
  eventDecoder
    |> Decode.map (\ev -> { message = tag ev, stopPropagation = options.stopPropagation, preventDefault = options.preventDefault })
    |> Svg.Events.custom event


defaultOptions : EventOptions
defaultOptions =
  { stopPropagation = False
  , preventDefault = True
  }

type alias EventOptions =
  { stopPropagation : Bool
  , preventDefault : Bool
  }

eventDecoder : Decoder Event
eventDecoder =
  Decode.map4 Event
    keys
    (Decode.field "changedTouches" <| touchListDecoder touchDecoder)
    (Decode.field "targetTouches" <| touchListDecoder touchDecoder)
    (Decode.field "touches" <| touchListDecoder touchDecoder)

touchDecoder : Decoder Touch
touchDecoder =
  Decode.map4 Touch
    (Decode.field "identifier" Decode.int)
    (Decode.map2 (\a b -> ( a, b ))
      (Decode.field "clientX" Decode.float)
      (Decode.field "clientY" Decode.float))
    (Decode.map2 (\a b -> ( a, b ))
      (Decode.field "pageX" Decode.float)
      (Decode.field "pageY" Decode.float))
    (Decode.map2 (\a b -> ( a, b ))
      (Decode.field "screenX" Decode.float)
      (Decode.field "screenY" Decode.float))

touchListDecoder : Decoder a -> Decoder (List a)
touchListDecoder =
  dynamicListOf

dynamicListOf : Decoder a -> Decoder (List a)
dynamicListOf itemDecoder =
  let
    decodeN n =
      List.range 0 (n - 1)
        |> List.map decodeOne
        |> all

    decodeOne n =
      Decode.field (String.fromInt n) itemDecoder
  in
  Decode.field "length" Decode.int
    |> Decode.andThen decodeN


all : List (Decoder a) -> Decoder (List a)
all =
  List.foldr (Decode.map2 (::)) (Decode.succeed [])

keys : Decoder Keys
keys =
  Decode.map3 Keys
    (Decode.field "altKey" Decode.bool)
    (Decode.field "ctrlKey" Decode.bool)
    (Decode.field "shiftKey" Decode.bool)