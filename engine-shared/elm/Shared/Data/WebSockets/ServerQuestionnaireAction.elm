module Shared.Data.WebSockets.ServerQuestionnaireAction exposing (ServerQuestionnaireAction(..), decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)


type ServerQuestionnaireAction
    = SetUserList (List OnlineUserInfo)
    | SetContent QuestionnaireEvent


decoder : Decoder ServerQuestionnaireAction
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerQuestionnaireAction
decoderByType actionType =
    case actionType of
        "SetUserList_ServerQuestionnaireAction" ->
            buildDecoder SetUserList (D.list OnlineUserInfo.decoder)

        "SetContent_ServerQuestionnaireAction" ->
            buildDecoder SetContent QuestionnaireEvent.decoder

        _ ->
            D.fail <| "Unknown ServerQuestionnaireAction: " ++ actionType


buildDecoder : (data -> action) -> Decoder data -> Decoder action
buildDecoder constructor dataDecoder =
    D.succeed constructor
        |> D.required "data" dataDecoder
