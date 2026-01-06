module Wizard.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetRepliesKnowledgeModelEditorMessage exposing
    ( SetRepliesKnowledgeModelEditorMessage
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectDetail.Reply as Reply exposing (Reply)


type alias SetRepliesKnowledgeModelEditorMessage =
    { uuid : Uuid
    , replies : Dict String Reply
    }


decoder : Decoder SetRepliesKnowledgeModelEditorMessage
decoder =
    D.succeed SetRepliesKnowledgeModelEditorMessage
        |> D.required "uuid" Uuid.decoder
        |> D.required "replies" (D.dict Reply.decoder)


encode : SetRepliesKnowledgeModelEditorMessage -> E.Value
encode action =
    E.object
        [ ( "uuid", Uuid.encode action.uuid )
        , ( "replies", E.dict identity Reply.encode action.replies )
        ]
