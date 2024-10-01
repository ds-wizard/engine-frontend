module Wizard.Common.Components.AIAssistant.Api exposing
    ( getLatestConversation
    , postQuestion
    )

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtFetch, jwtGet)
import Uuid exposing (Uuid)
import Wizard.Common.Components.AIAssistant.Models.Answer as Answer exposing (Answer)
import Wizard.Common.Components.AIAssistant.Models.Conversation as Conversation exposing (Conversation)
import Wizard.Common.Components.AIAssistant.Models.Question as Question exposing (Question)


getLatestConversation : AbstractAppState a -> ToMsg Conversation msg -> Cmd msg
getLatestConversation =
    jwtGet "/conversations/latest?application=wizard" Conversation.decoder


postQuestion : Uuid -> Question -> AbstractAppState a -> ToMsg Answer msg -> Cmd msg
postQuestion uuid question =
    jwtFetch ("/conversations/" ++ Uuid.toString uuid ++ "?application=wizard") Answer.decoder (Question.encode question)
