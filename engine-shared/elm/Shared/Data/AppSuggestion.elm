module Shared.Data.AppSuggestion exposing
    ( AppSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias AppSuggestion =
    { uuid : Uuid
    , name : String
    , logoUrl : Maybe String
    , primaryColor : Maybe String
    , clientUrl : String
    }


decoder : Decoder AppSuggestion
decoder =
    D.succeed AppSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "logoUrl" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "clientUrl" D.string
