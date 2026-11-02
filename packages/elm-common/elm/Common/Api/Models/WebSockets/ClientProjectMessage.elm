module Common.Api.Models.WebSockets.ClientProjectMessage exposing (ClientProjectMessage(..), encode)

import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Json.Encode as E


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
