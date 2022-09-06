module Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetPhaseData exposing
    ( SetPhaseData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.SummaryReport.AnsweredIndicationData as AnsweredIndicationData exposing (AnsweredIndicationData)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias SetPhaseData =
    { uuid : Uuid
    , phaseUuid : Maybe Uuid
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    , phasesAnsweredIndication : AnsweredIndicationData
    }


encode : SetPhaseData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "SetPhaseEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "phaseUuid", E.maybe Uuid.encode data.phaseUuid )
        , ( "phasesAnsweredIndication", AnsweredIndicationData.encode data.phasesAnsweredIndication )
        ]


decoder : Decoder SetPhaseData
decoder =
    D.succeed SetPhaseData
        |> D.required "uuid" Uuid.decoder
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.hardcoded AnsweredIndicationData.empty
