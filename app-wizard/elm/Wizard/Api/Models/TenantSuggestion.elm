module Wizard.Api.Models.TenantSuggestion exposing
    ( TenantSuggestion
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias TenantSuggestion =
    { uuid : Uuid
    , name : String
    , logoUrl : Maybe String
    , primaryColor : Maybe String
    , clientUrl : String
    }


decoder : Decoder TenantSuggestion
decoder =
    D.succeed TenantSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "logoUrl" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.string)
        |> D.required "clientUrl" D.string
