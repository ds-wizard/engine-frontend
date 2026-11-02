module Common.Api.Models.WebSockets.ServerKnowledgeModelEditorMessage exposing
    ( ServerKnowledgeModelEditorMessage(..)
    , decoder
    )

import Common.Api.Models.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Common.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetContentKnowledgeModelEditorMessage as SetContentKnowledgeModelEditorMessage exposing (SetContentKnowledgeModelEditorMessage)
import Common.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetRepliesKnowledgeModelEditorMessage as SetRepliesKnowledgeModelEditorMessage exposing (SetRepliesKnowledgeModelEditorMessage)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type ServerKnowledgeModelEditorMessage
    = SetUserList (List OnlineUserInfo)
    | SetContent SetContentKnowledgeModelEditorMessage
    | SetReplies SetRepliesKnowledgeModelEditorMessage


decoder : Decoder ServerKnowledgeModelEditorMessage
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerKnowledgeModelEditorMessage
decoderByType actionType =
    case actionType of
        "SetUserList_ServerKnowledgeModelEditorMessage" ->
            buildDecoder SetUserList (D.list OnlineUserInfo.decoder)

        "SetContent_ServerKnowledgeModelEditorMessage" ->
            buildDecoder SetContent SetContentKnowledgeModelEditorMessage.decoder

        "SetReplies_ServerKnowledgeModelEditorMessage" ->
            buildDecoder SetReplies SetRepliesKnowledgeModelEditorMessage.decoder

        _ ->
            D.fail <| "Unknown ServerKnowledgeModelEditorMessage: " ++ actionType


buildDecoder : (data -> action) -> Decoder data -> Decoder action
buildDecoder constructor dataDecoder =
    D.succeed constructor
        |> D.required "data" dataDecoder
