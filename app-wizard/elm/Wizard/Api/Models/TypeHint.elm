module Wizard.Api.Models.TypeHint exposing (TypeHint, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Value as JsonValue exposing (JsonValue)


type alias TypeHint =
    { value : String
    , valueForSelection : Maybe String
    , raw : JsonValue
    }


decoder : Decoder TypeHint
decoder =
    D.succeed TypeHint
        |> D.required "value" D.string
        |> D.required "valueForSelection" (D.maybe D.string)
        |> D.required "raw" JsonValue.decoder
