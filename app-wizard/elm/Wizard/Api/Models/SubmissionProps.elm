module Wizard.Api.Models.SubmissionProps exposing
    ( SubmissionProps
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias SubmissionProps =
    { id : String
    , name : String
    , values : Dict String String
    }


decoder : Decoder SubmissionProps
decoder =
    D.succeed SubmissionProps
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "values" (D.dict D.string)
