module Wizard.Api.Models.WebSockets.ClientKnowledgeModelEditorMessage exposing
    ( ClientKnowledgeModelEditorMessage(..)
    , encode
    )

import Json.Encode as E
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetContentKnowledgeModelEditorMessage as SetContentKnowledgeModelEditorMessage exposing (SetContentKnowledgeModelEditorMessage)
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetRepliesKnowledgeModelEditorMessage as SetRepliesKnowledgeModelEditorMessage exposing (SetRepliesKnowledgeModelEditorMessage)


type ClientKnowledgeModelEditorMessage
    = SetContent SetContentKnowledgeModelEditorMessage
    | SetReplies SetRepliesKnowledgeModelEditorMessage


encode : ClientKnowledgeModelEditorMessage -> E.Value
encode action =
    case action of
        SetContent event ->
            encodeMessageData "SetContent_ClientKnowledgeModelEditorMessage" (SetContentKnowledgeModelEditorMessage.encode event)

        SetReplies event ->
            encodeMessageData "SetReplies_ClientKnowledgeModelEditorMessage" (SetRepliesKnowledgeModelEditorMessage.encode event)


encodeMessageData : String -> E.Value -> E.Value
encodeMessageData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
