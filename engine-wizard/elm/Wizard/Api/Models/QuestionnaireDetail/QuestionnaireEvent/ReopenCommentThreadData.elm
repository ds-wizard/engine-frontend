module Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.ReopenCommentThreadData exposing
    ( ReopenCommentThreadData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)


type alias ReopenCommentThreadData =
    { uuid : Uuid
    , path : String
    , threadUuid : Uuid
    , private : Bool
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    , commentCount : Int
    }


encode : ReopenCommentThreadData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "ReopenCommentThreadEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "threadUuid", Uuid.encode data.threadUuid )
        , ( "private", E.bool data.private )
        , ( "commentCount", E.int data.commentCount )
        ]


decoder : Decoder ReopenCommentThreadData
decoder =
    D.succeed ReopenCommentThreadData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "threadUuid" Uuid.decoder
        |> D.hardcoded False
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "commentCount" D.int
