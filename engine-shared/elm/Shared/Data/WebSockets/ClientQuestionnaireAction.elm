module Shared.Data.WebSockets.ClientQuestionnaireAction exposing (ClientQuestionnaireAction(..), encode)

import Json.Encode as E
import Shared.Data.WebSockets.QuestionnaireAction.ClearReplyData as ClearAnswerData exposing (ClearReplyData)
import Shared.Data.WebSockets.QuestionnaireAction.SetLabelsData as SetLablesData exposing (SetLabelsData)
import Shared.Data.WebSockets.QuestionnaireAction.SetLevelData as SetLevelData exposing (SetLevelData)
import Shared.Data.WebSockets.QuestionnaireAction.SetReplyData as SetReplyData exposing (SetReplyData)


type ClientQuestionnaireAction
    = SetReply SetReplyData
    | ClearReply ClearReplyData
    | SetLevel SetLevelData
    | SetLabels SetLabelsData


encode : ClientQuestionnaireAction -> E.Value
encode action =
    case action of
        SetReply data ->
            encodeActionData "SetReply_ClientQuestionnaireAction" (SetReplyData.encode data)

        ClearReply data ->
            encodeActionData "ClearReply_ClientQuestionnaireAction" (ClearAnswerData.encode data)

        SetLevel data ->
            encodeActionData "SetLevel_ClientQuestionnaireAction" (SetLevelData.encode data)

        SetLabels data ->
            encodeActionData "SetLabels_ClientQuestionnaireAction" (SetLablesData.encode data)


encodeActionData : String -> E.Value -> E.Value
encodeActionData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
