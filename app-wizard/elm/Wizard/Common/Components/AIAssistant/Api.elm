module Wizard.Common.Components.AIAssistant.Api exposing
    ( getLatestConversation
    , postQuestion
    )

import Shared.Api.Request as Request exposing (ServerInfo, ToMsg)
import Uuid exposing (Uuid)
import Wizard.Common.Components.AIAssistant.Models.Answer as Answer exposing (Answer)
import Wizard.Common.Components.AIAssistant.Models.Conversation as Conversation exposing (Conversation)
import Wizard.Common.Components.AIAssistant.Models.Question as Question exposing (Question)


getLatestConversation : ServerInfo -> ToMsg Conversation msg -> Cmd msg
getLatestConversation serverInfo =
    Request.get serverInfo "/conversations/latest?application=wizard" Conversation.decoder


postQuestion : ServerInfo -> Uuid -> Question -> ToMsg Answer msg -> Cmd msg
postQuestion serverInfo uuid question =
    Request.post serverInfo ("/conversations/" ++ Uuid.toString uuid ++ "?application=wizard") Answer.decoder (Question.encode question)
