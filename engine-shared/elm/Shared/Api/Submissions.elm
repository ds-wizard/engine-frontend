module Shared.Api.Submissions exposing (..)

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtFetch)
import Shared.Data.Submission as Submission exposing (Submission)


postSubmission : String -> String -> AbstractAppState a -> ToMsg Submission msg -> Cmd msg
postSubmission serviceId documentUuid =
    let
        body =
            E.object
                [ ( "serviceId", E.string serviceId )
                , ( "docUuid", E.string documentUuid )
                ]
    in
    jwtFetch "/submissions" Submission.decoder body
