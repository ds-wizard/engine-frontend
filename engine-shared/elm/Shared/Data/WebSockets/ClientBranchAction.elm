module Shared.Data.WebSockets.ClientBranchAction exposing
    ( ClientBranchAction(..)
    , encode
    )

import Json.Encode as E
import Shared.Data.WebSockets.BranchAction.SetContentBranchAction as SetContentBranchAction exposing (SetContentBranchAction)


type ClientBranchAction
    = SetContent SetContentBranchAction


encode : ClientBranchAction -> E.Value
encode action =
    case action of
        SetContent event ->
            encodeActionData "SetContent_ClientBranchAction" (SetContentBranchAction.encode event)


encodeActionData : String -> E.Value -> E.Value
encodeActionData actionType data =
    E.object
        [ ( "type", E.string actionType )
        , ( "data", data )
        ]
