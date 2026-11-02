module Common.Api.Models.DevOperation.DevOperationParameter exposing
    ( DevOperationParameter
    , decoder
    )

import Common.Api.Models.DevOperation.DevOperationParameterType as AdminOperationParameterType exposing (DevOperationParameterType)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias DevOperationParameter =
    { name : String
    , type_ : DevOperationParameterType
    }


decoder : Decoder DevOperationParameter
decoder =
    D.succeed DevOperationParameter
        |> D.required "name" D.string
        |> D.required "type" AdminOperationParameterType.decoder
