module Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent.DeleteCommentThreadData exposing
    ( DeleteCommentThreadData
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


type alias DeleteCommentThreadData =
    { uuid : Uuid
    , path : String
    , threadUuid : Uuid
    , private : Bool
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : DeleteCommentThreadData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "DeleteCommentThreadEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "private", E.bool data.private )
        , ( "threadUuid", Uuid.encode data.threadUuid )
        ]


decoder : Decoder DeleteCommentThreadData
decoder =
    D.succeed DeleteCommentThreadData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "threadUuid" Uuid.decoder
        |> D.hardcoded False
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
