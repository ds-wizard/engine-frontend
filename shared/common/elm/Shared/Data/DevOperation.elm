module Shared.Data.DevOperation exposing
    ( DevOperation
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.DevOperation.DevOperationParameter as AdminOperationParameter exposing (AdminOperationParameter)


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
