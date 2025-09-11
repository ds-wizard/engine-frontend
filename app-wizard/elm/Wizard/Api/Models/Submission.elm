module Wizard.Api.Models.Submission exposing (Submission, compare, decoder, getReturnedData, visibleName)

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Time.Extra as Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.Submission.SubmissionState as SubmissionState exposing (SubmissionState)


type alias Submission =
    { uuid : Uuid
    , state : SubmissionState
    , location : Maybe String
    , returnedData : Maybe String
    , serviceId : String
    , serviceName : Maybe String
    , documentUuid : Uuid
    , createdBy : UserSuggestion
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Decoder Submission
decoder =
    D.succeed Submission
        |> D.required "uuid" Uuid.decoder
        |> D.required "state" SubmissionState.decoder
        |> D.required "location" (D.maybe D.string)
        |> D.required "returnedData" (D.maybe D.string)
        |> D.required "serviceId" D.string
        |> D.required "serviceName" (D.maybe D.string)
        |> D.required "documentUuid" Uuid.decoder
        |> D.required "createdBy" UserSuggestion.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime


visibleName : Submission -> String
visibleName submission =
    Maybe.withDefault submission.serviceId submission.serviceName


compare : Submission -> Submission -> Order
compare a b =
    Time.compare b.updatedAt a.updatedAt


getReturnedData : Submission -> String
getReturnedData =
    .returnedData
        >> Maybe.withDefault ""
        >> String.replace "Response Body: \"" "Response Body:\n\n"
        >> String.slice 0 -1
        >> String.replace "\\\\" "\\"
        >> String.replace "\\n" "\n"
        >> String.replace "\\\"" "\""
