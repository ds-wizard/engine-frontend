module Shared.Data.Submission exposing (Submission, compare, decoder, visibleName)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.Submission.SubmissionState as SubmissionState exposing (SubmissionState)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


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
    Basics.compare (Time.posixToMillis b.updatedAt) (Time.posixToMillis a.updatedAt)
