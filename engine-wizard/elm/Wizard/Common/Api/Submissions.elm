module Wizard.Common.Api.Submissions exposing (..)

import Json.Encode as E
import Wizard.Common.Api exposing (ToMsg, jwtFetch)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Common.Submission as Submission exposing (Submission)


postSubmission : String -> String -> AppState -> ToMsg Submission msg -> Cmd msg
postSubmission serviceId documentUuid =
    let
        body =
            E.object
                [ ( "serviceId", E.string serviceId )
                , ( "docUuid", E.string documentUuid )
                ]
    in
    jwtFetch "/submissions" Submission.decoder body
