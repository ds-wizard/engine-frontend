module Wizard.Api.Models.WebSockets.ClientBranchAction exposing
    ( ClientBranchAction(..)
    , encode
    )

import Json.Encode as E
import Wizard.Api.Models.WebSockets.BranchAction.SetContentBranchAction as SetContentBranchAction exposing (SetContentBranchAction)
import Wizard.Api.Models.WebSockets.BranchAction.SetRepliesBranchAction as SetRepliesBranchAction exposing (SetRepliesBranchAction)


type ClientBranchAction
    = SetContent SetContentBranchAction
    | SetReplies SetRepliesBranchAction


encode : ClientBranchAction -> E.Value
encode action =
    case action of
        SetContent event ->
            encodeActionData "SetContent_ClientBranchAction" (SetContentBranchAction.encode event)

        SetReplies event ->
            encodeActionData "SetReplies_ClientBranchAction" (SetRepliesBranchAction.encode event)


encodeActionData : String -> E.Value -> E.Value
encodeActionData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
