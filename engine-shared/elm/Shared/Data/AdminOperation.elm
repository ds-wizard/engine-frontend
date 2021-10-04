module Shared.Data.AdminOperation exposing
    ( AdminOperation
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.AdminOperation.AdminOperationParameter as AdminOperationParameter exposing (AdminOperationParameter)


type alias AdminOperation =
    { name : String
    , description : Maybe String
    , parameters : List AdminOperationParameter
    }


decoder : Decoder AdminOperation
decoder =
    D.succeed AdminOperation
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "parameters" (D.list AdminOperationParameter.decoder)
