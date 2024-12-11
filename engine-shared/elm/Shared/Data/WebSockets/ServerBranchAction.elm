module Shared.Data.WebSockets.ServerBranchAction exposing
    ( ServerBranchAction(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Data.WebSockets.BranchAction.SetContentBranchAction as SetContentBranchAction exposing (SetContentBranchAction)
import Shared.Data.WebSockets.BranchAction.SetRepliesBranchAction as SetRepliesBranchAction exposing (SetRepliesBranchAction)


type ServerBranchAction
    = SetUserList (List OnlineUserInfo)
    | SetContent SetContentBranchAction
    | SetReplies SetRepliesBranchAction


decoder : Decoder ServerBranchAction
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerBranchAction
decoderByType actionType =
    case actionType of
        "SetUserList_ServerBranchAction" ->
            buildDecoder SetUserList (D.list OnlineUserInfo.decoder)

        "SetContent_ServerBranchAction" ->
            buildDecoder SetContent SetContentBranchAction.decoder

        "SetReplies_ServerBranchAction" ->
            buildDecoder SetReplies SetRepliesBranchAction.decoder

        _ ->
            D.fail <| "Unknown ServerBranchAction: " ++ actionType


buildDecoder : (data -> action) -> Decoder data -> Decoder action
buildDecoder constructor dataDecoder =
    D.succeed constructor
        |> D.required "data" dataDecoder
