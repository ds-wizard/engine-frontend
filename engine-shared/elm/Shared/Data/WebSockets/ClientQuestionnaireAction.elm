module Shared.Data.WebSockets.ClientQuestionnaireAction exposing (ClientQuestionnaireAction(..), encode)

import Json.Encode as E
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)


type ClientQuestionnaireAction
    = SetContent QuestionnaireEvent


encode : ClientQuestionnaireAction -> E.Value
encode action =
    case action of
        SetContent event ->
            encodeActionData "SetContent_ClientQuestionnaireAction" (QuestionnaireEvent.encode event)


encodeActionData : String -> E.Value -> E.Value
encodeActionData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
