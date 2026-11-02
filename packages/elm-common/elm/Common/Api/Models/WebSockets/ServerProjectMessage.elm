module Common.Api.Models.WebSockets.ServerProjectMessage exposing (ServerProjectMessage(..), decoder)

import Common.Api.Models.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectFileSimple as ProjectFileSimple exposing (ProjectFileSimple)
import Common.Api.Models.WebSockets.ProjectMessage.SetProjectData as SetProjectData exposing (SetProjectData)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type ServerProjectMessage
    = SetUserList (List OnlineUserInfo)
    | SetContent ProjectEvent
    | SetProject SetProjectData
    | AddFile ProjectFileSimple


decoder : Decoder ServerProjectMessage
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ServerProjectMessage
decoderByType actionType =
    case actionType of
        "SetUserList_ServerProjectMessage" ->
            buildDecoder SetUserList (D.list OnlineUserInfo.decoder)

        "SetContent_ServerProjectMessage" ->
            buildDecoder SetContent ProjectEvent.decoder

        "SetProject_ServerProjectMessage" ->
            buildDecoder SetProject SetProjectData.decoder

        "AddFile_ServerProjectMessage" ->
            buildDecoder AddFile ProjectFileSimple.decoder

        _ ->
            D.fail <| "Unknown ServerProjectMessage: " ++ actionType


buildDecoder : (data -> action) -> Decoder data -> Decoder action
buildDecoder constructor dataDecoder =
    D.succeed constructor
        |> D.required "data" dataDecoder
