module Shared.Data.WebSockets.QuestionnaireAction.SetReplyData exposing
    ( SetReplyData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.QuestionnaireDetail.ReplyValue as ReplyValue exposing (ReplyValue)
import Uuid exposing (Uuid)


type alias SetReplyData =
    { uuid : Uuid
    , path : String
    , value : ReplyValue
    }


encode : SetReplyData -> E.Value
encode data =
    E.object
        [ ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "value", ReplyValue.encode data.value )
        ]


decoder : Decoder SetReplyData
decoder =
    D.succeed SetReplyData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "value" ReplyValue.decoder
