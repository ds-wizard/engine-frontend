module Wizard.Common.Components.AIAssistant.Models.Conversation exposing
    ( Conversation
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Wizard.Common.Components.AIAssistant.Models.Message as Message exposing (Message)


type alias Conversation =
    { uuid : Uuid
    , messages : List Message
    }


decoder : Decoder Conversation
decoder =
    D.succeed Conversation
        |> D.required "uuid" Uuid.decoder
        |> D.required "messages" (D.list Message.decoder)
