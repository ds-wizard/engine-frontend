module Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.AssignCommentThreadData exposing (AssignCommentThreadData, decoder, encode)

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Time
import Uuid exposing (Uuid)


type alias AssignCommentThreadData =
    { uuid : Uuid
    , path : String
    , threadUuid : Uuid
    , private : Bool
    , assignedTo : Maybe UserSuggestion
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : AssignCommentThreadData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "AssignCommentThreadEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "threadUuid", Uuid.encode data.threadUuid )
        , ( "private", E.bool data.private )
        , ( "assignedTo", E.maybe UserSuggestion.encode data.assignedTo )
        ]


decoder : Decoder AssignCommentThreadData
decoder =
    D.succeed AssignCommentThreadData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "threadUuid" Uuid.decoder
        |> D.required "private" D.bool
        |> D.required "assignedTo" (D.maybe UserSuggestion.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
