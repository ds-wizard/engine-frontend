module Common.Data.DevOperation.DevOperationParameter exposing
    ( AdminOperationParameter
    , decoder
    )

import Common.Data.DevOperation.DevOperationParameterType as AdminOperationParameterType exposing (DevOperationParameterType)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AdminOperationParameter =
    { name : String
    , type_ : DevOperationParameterType
    }


decoder : Decoder AdminOperationParameter
decoder =
    D.succeed AdminOperationParameter
        |> D.required "name" D.string
        |> D.required "type" AdminOperationParameterType.decoder
