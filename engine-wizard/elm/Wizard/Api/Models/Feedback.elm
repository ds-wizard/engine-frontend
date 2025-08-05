module Wizard.Api.Models.Feedback exposing
    ( Feedback
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Feedback =
    { title : String
    , issueId : Int
    , issueUrl : String
    }


decoder : Decoder Feedback
decoder =
    D.succeed Feedback
        |> D.required "title" D.string
        |> D.required "issueId" D.int
        |> D.required "issueUrl" D.string
