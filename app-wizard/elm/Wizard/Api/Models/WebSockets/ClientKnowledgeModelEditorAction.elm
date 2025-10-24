module Wizard.Api.Models.WebSockets.ClientKnowledgeModelEditorAction exposing
    ( ClientKnowledgeModelEditorAction(..)
    , encode
    )

import Json.Encode as E
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetContentKnowledgeModelEditorAction as SetContentKnowledgeModelEditorAction exposing (SetContentKnowledgeModelEditorAction)
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetRepliesKnowledgeModelEditorAction as SetRepliesKnowledgeModelEditorAction exposing (SetRepliesKnowledgeModelEditorAction)


type ClientKnowledgeModelEditorAction
    = SetContent SetContentKnowledgeModelEditorAction
    | SetReplies SetRepliesKnowledgeModelEditorAction


encode : ClientKnowledgeModelEditorAction -> E.Value
encode action =
    case action of
        SetContent event ->
            encodeActionData "SetContent_ClientKnowledgeModelEditorAction" (SetContentKnowledgeModelEditorAction.encode event)

        SetReplies event ->
            encodeActionData "SetReplies_ClientKnowledgeModelEditorAction" (SetRepliesKnowledgeModelEditorAction.encode event)


encodeActionData : String -> E.Value -> E.Value
encodeActionData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
