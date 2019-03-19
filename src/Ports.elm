port module Ports exposing
  ( encodeUri
  , onUriEncoded
  , decodeUri
  , onUriDecoded
  )

port encodeUri : String -> Cmd msg
port onUriEncoded : (String -> msg) -> Sub msg

port decodeUri : String -> Cmd msg
port onUriDecoded : (String -> msg) -> Sub msg