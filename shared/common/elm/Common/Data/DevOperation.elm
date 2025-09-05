module Common.Data.DevOperation exposing
    ( DevOperation
    , decoder
    )

import Common.Data.DevOperation.DevOperationParameter as AdminOperationParameter exposing (AdminOperationParameter)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias DevOperation =
    { name : String
    , description : Maybe String
    , parameters : List AdminOperationParameter
    }


decoder : Decoder DevOperation
decoder =
    D.succeed DevOperation
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "parameters" (D.list AdminOperationParameter.decoder)
