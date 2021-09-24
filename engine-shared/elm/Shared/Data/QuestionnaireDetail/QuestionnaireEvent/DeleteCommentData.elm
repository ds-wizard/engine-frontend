module Shared.Data.QuestionnaireDetail.QuestionnaireEvent.DeleteCommentData exposing
    ( DeleteCommentData
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


type alias DeleteCommentData =
    { uuid : Uuid
    , path : String
    , threadUuid : Uuid
    , commentUuid : Uuid
    , private : Bool
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : DeleteCommentData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "DeleteCommentEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "private", E.bool data.private )
        , ( "threadUuid", Uuid.encode data.threadUuid )
        , ( "commentUuid", Uuid.encode data.commentUuid )
        ]


decoder : Decoder DeleteCommentData
decoder =
    D.succeed DeleteCommentData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "threadUuid" Uuid.decoder
        |> D.required "commentUuid" Uuid.decoder
        |> D.hardcoded False
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
