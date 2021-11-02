module Shared.Data.AdminOperationExecutionResult exposing
    ( AdminOperationExecutionResult
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AdminOperationExecutionResult =
    { output : String }


decoder : Decoder AdminOperationExecutionResult
decoder =
    D.succeed AdminOperationExecutionResult
        |> D.required "output" D.string
