module Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetPhaseData exposing
    ( SetPhaseData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias SetPhaseData =
    { uuid : Uuid
    , phaseUuid : Uuid
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : SetPhaseData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "SetPhaseEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "phaseUuid", Uuid.encode data.phaseUuid )
        ]


decoder : Decoder SetPhaseData
decoder =
    D.succeed SetPhaseData
        |> D.required "uuid" Uuid.decoder
        |> D.required "phaseUuid" Uuid.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
