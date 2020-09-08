module Shared.Data.WebSockets.ServerQuestionnaireAction exposing (ServerQuestionnaireAction(..), decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Data.WebSockets.QuestionnaireAction.ClearReplyData as ClearReplyData exposing (ClearReplyData)
import Shared.Data.WebSockets.QuestionnaireAction.SetLabelsData as SetLabelsData exposing (SetLabelsData)
import Shared.Data.WebSockets.QuestionnaireAction.SetLevelData as SetLevelData exposing (SetLevelData)
import Shared.Data.WebSockets.QuestionnaireAction.SetReplyData as SetAnswerData exposing (SetReplyData)


type ServerQuestionnaireAction
    = SetUserList (List OnlineUserInfo)
    | SetReply SetReplyData
    | ClearReply ClearReplyData
    | SetLevel SetLevelData
    | SetLabels SetLabelsData


decoder : Decoder ServerQuestionnaireAction
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerQuestionnaireAction
decoderByType actionType =
    case actionType of
        "SetUserList_ServerQuestionnaireAction" ->
            buildDecoder SetUserList (D.list OnlineUserInfo.decoder)

        "SetReply_ServerQuestionnaireAction" ->
            buildDecoder SetReply SetAnswerData.decoder

        "ClearReply_ServerQuestionnaireAction" ->
            buildDecoder ClearReply ClearReplyData.decoder

        "SetLevel_ServerQuestionnaireAction" ->
            buildDecoder SetLevel SetLevelData.decoder

        "SetLabels_ServerQuestionnaireAction" ->
            buildDecoder SetLabels SetLabelsData.decoder

        _ ->
            D.fail <| "Unknown ServerQuestionnaireAction: " ++ actionType


buildDecoder : (data -> action) -> Decoder data -> Decoder action
buildDecoder constructor dataDecoder =
    D.succeed constructor
        |> D.required "data" dataDecoder
