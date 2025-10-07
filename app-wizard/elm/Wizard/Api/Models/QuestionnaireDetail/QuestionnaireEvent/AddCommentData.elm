module Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.AddCommentData exposing
    ( AddCommentData
    , decoder
    , encode
    , toComment
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireDetail.Comment exposing (Comment)


type alias AddCommentData =
    { uuid : Uuid
    , path : String
    , threadUuid : Uuid
    , newThread : Bool
    , commentUuid : Uuid
    , text : String
    , private : Bool
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : AddCommentData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "AddCommentEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "threadUuid", Uuid.encode data.threadUuid )
        , ( "newThread", E.bool data.newThread )
        , ( "commentUuid", Uuid.encode data.commentUuid )
        , ( "text", E.string data.text )
        , ( "private", E.bool data.private )
        ]


decoder : Decoder AddCommentData
decoder =
    D.succeed AddCommentData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "threadUuid" Uuid.decoder
        |> D.hardcoded False
        |> D.required "commentUuid" Uuid.decoder
        |> D.required "text" D.string
        |> D.required "private" D.bool
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)


toComment : AddCommentData -> Comment
toComment data =
    { uuid = data.commentUuid
    , text = data.text
    , createdBy = data.createdBy
    , createdAt = data.createdAt
    , updatedAt = data.createdAt
    }
