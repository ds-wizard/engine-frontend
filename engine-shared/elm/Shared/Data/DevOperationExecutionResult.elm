module Shared.Data.DevOperationExecutionResult exposing
    ( DevOperationExecutionResult
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias DevOperationExecutionResult =
    { output : String }


decoder : Decoder DevOperationExecutionResult
decoder =
    D.succeed DevOperationExecutionResult
        |> D.required "output" D.string
