module Shared.Data.WebSockets.QuestionnaireAction.ClearReplyData exposing
    ( ClearReplyData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias ClearReplyData =
    { uuid : Uuid
    , path : String
    }


encode : ClearReplyData -> E.Value
encode data =
    E.object
        [ ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        ]


decoder : Decoder ClearReplyData
decoder =
    D.succeed ClearReplyData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
