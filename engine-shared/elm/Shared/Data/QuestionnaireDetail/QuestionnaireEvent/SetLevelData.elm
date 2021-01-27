module Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetLevelData exposing
    ( SetLevelData
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


type alias SetLevelData =
    { uuid : Uuid
    , level : Int
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : SetLevelData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "SetLevelEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "level", E.int data.level )
        ]


decoder : Decoder SetLevelData
decoder =
    D.succeed SetLevelData
        |> D.required "uuid" Uuid.decoder
        |> D.required "level" D.int
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
