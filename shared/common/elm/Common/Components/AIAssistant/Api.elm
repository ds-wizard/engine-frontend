module Common.Components.AIAssistant.Api exposing
    ( getLatestConversation
    , postQuestion
    )

import Common.Api.Request as Request exposing (ServerInfo, ToMsg)
import Common.Components.AIAssistant.Models.Answer as Answer exposing (Answer)
import Common.Components.AIAssistant.Models.Conversation as Conversation exposing (Conversation)
import Common.Components.AIAssistant.Models.Question as Question exposing (Question)
import Uuid exposing (Uuid)


getLatestConversation : ServerInfo -> ToMsg Conversation msg -> Cmd msg
getLatestConversation serverInfo =
    Request.get serverInfo "/conversations/latest?application=wizard" Conversation.decoder


postQuestion : ServerInfo -> Uuid -> Question -> ToMsg Answer msg -> Cmd msg
postQuestion serverInfo uuid question =
    Request.post serverInfo ("/conversations/" ++ Uuid.toString uuid ++ "?application=wizard") Answer.decoder (Question.encode question)
