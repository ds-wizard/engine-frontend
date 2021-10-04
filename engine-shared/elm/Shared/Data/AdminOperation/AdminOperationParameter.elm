module Shared.Data.AdminOperation.AdminOperationParameter exposing
    ( AdminOperationParameter
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.AdminOperation.AdminOperationParameterType as AdminOperationParameterType exposing (AdminOperationParameterType)


type alias AdminOperationParameter =
    { name : String
    , type_ : AdminOperationParameterType
    }


decoder : Decoder AdminOperationParameter
decoder =
    D.succeed AdminOperationParameter
        |> D.required "name" D.string
        |> D.required "type" AdminOperationParameterType.decoder
