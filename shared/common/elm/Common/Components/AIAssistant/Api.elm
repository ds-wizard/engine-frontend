module Common.Components.AIAssistant.Api exposing
    ( getLatestConversation
    , postQuestion
    )

import Common.Api.Request as Request exposing (ServerInfo, ToMsg)
import Common.Components.AIAssistant.Models.Answer as Answer exposing (Answer)
import Common.Components.AIAssistant.Models.Conversation as Conversation exposing (Conversation)
import Common.Components.AIAssistant.Models.Question as Question exposing (Question)
import Uuid exposing (Uuid)


getLatestConversation : ServerInfo -> String -> ToMsg Conversation msg -> Cmd msg
getLatestConversation serverInfo application =
    Request.get serverInfo ("/conversations/latest?application=" ++ application) Conversation.decoder


postQuestion : ServerInfo -> String -> Uuid -> Question -> ToMsg Answer msg -> Cmd msg
postQuestion serverInfo appName uuid question =
    Request.post serverInfo ("/conversations/" ++ Uuid.toString uuid ++ "?application=" ++ appName) Answer.decoder (Question.encode question)
