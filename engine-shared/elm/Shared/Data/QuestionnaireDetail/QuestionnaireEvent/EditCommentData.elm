module Shared.Data.QuestionnaireDetail.QuestionnaireEvent.EditCommentData exposing
    ( EditCommentData
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


type alias EditCommentData =
    { uuid : Uuid
    , path : String
    , threadUuid : Uuid
    , commentUuid : Uuid
    , text : String
    , private : Bool
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : EditCommentData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "EditCommentEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "threadUuid", Uuid.encode data.threadUuid )
        , ( "commentUuid", Uuid.encode data.commentUuid )
        , ( "text", E.string data.text )
        , ( "private", E.bool data.private )
        ]


decoder : Decoder EditCommentData
decoder =
    D.succeed EditCommentData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "threadUuid" Uuid.decoder
        |> D.required "commentUuid" Uuid.decoder
        |> D.required "text" D.string
        |> D.hardcoded False
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
