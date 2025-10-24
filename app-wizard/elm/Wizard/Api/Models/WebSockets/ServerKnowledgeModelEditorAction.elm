module Wizard.Api.Models.WebSockets.ServerKnowledgeModelEditorAction exposing
    ( ServerKnowledgeModelEditorAction(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetContentKnowledgeModelEditorAction as SetContentKnowledgeModelEditorAction exposing (SetContentKnowledgeModelEditorAction)
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetRepliesKnowledgeModelEditorAction as SetRepliesKnowledgeModelEditorAction exposing (SetRepliesKnowledgeModelEditorAction)


type ServerKnowledgeModelEditorAction
    = SetUserList (List OnlineUserInfo)
    | SetContent SetContentKnowledgeModelEditorAction
    | SetReplies SetRepliesKnowledgeModelEditorAction


decoder : Decoder ServerKnowledgeModelEditorAction
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerKnowledgeModelEditorAction
decoderByType actionType =
    case actionType of
        "SetUserList_ServerKnowledgeModelEditorAction" ->
            buildDecoder SetUserList (D.list OnlineUserInfo.decoder)

        "SetContent_ServerKnowledgeModelEditorAction" ->
            buildDecoder SetContent SetContentKnowledgeModelEditorAction.decoder

        "SetReplies_ServerKnowledgeModelEditorAction" ->
            buildDecoder SetReplies SetRepliesKnowledgeModelEditorAction.decoder

        _ ->
            D.fail <| "Unknown ServerKnowledgeModelEditorAction: " ++ actionType


buildDecoder : (data -> action) -> Decoder data -> Decoder action
buildDecoder constructor dataDecoder =
    D.succeed constructor
        |> D.required "data" dataDecoder
