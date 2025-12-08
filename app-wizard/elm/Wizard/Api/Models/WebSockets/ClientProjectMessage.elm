module Wizard.Api.Models.WebSockets.ClientProjectMessage exposing (ClientProjectMessage(..), encode)

import Json.Encode as E
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)


type ClientProjectMessage
    = SetContent ProjectEvent


encode : ClientProjectMessage -> E.Value
encode action =
    case action of
        SetContent event ->
            encodeMessageData "SetContent_ClientProjectMessage" (ProjectEvent.encode event)


encodeMessageData : String -> E.Value -> E.Value
encodeMessageData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
