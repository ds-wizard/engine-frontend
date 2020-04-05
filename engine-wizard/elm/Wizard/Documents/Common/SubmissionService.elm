module Wizard.Documents.Common.SubmissionService exposing (SubmissionService, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias SubmissionService =
    { id : String
    , name : String
    , description : String
    }


decoder : Decoder SubmissionService
decoder =
    D.succeed SubmissionService
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
