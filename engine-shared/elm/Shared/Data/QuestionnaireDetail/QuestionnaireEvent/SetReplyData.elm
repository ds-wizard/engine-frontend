module Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData exposing
    ( SetReplyData
    , decoder
    , encode
    , toReply
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.QuestionnaireDetail.Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias SetReplyData =
    { uuid : Uuid
    , path : String
    , value : ReplyValue
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : SetReplyData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "SetReplyEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "value", ReplyValue.encode data.value )
        ]


decoder : Decoder SetReplyData
decoder =
    D.succeed SetReplyData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "value" ReplyValue.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)


toReply : SetReplyData -> Reply
toReply data =
    { value = data.value
    , createdAt = data.createdAt
    , createdBy = data.createdBy
    }
