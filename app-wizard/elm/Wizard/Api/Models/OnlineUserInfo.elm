module Wizard.Api.Models.OnlineUserInfo exposing
    ( AnonymousData
    , LoggedData
    , OnlineUserInfo(..)
    , decoder
    , getUuid
    , matchUuid
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type OnlineUserInfo
    = Logged LoggedData
    | Anonymous AnonymousData


type alias LoggedData =
    { uuid : Uuid
    , firstName : String
    , lastName : String
    , gravatarHash : String
    , imageUrl : Maybe String
    , colorNumber : Int
    }


type alias AnonymousData =
    { avatarNumber : Int
    , colorNumber : Int
    }


decoder : Decoder OnlineUserInfo
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder OnlineUserInfo
decoderByType userType =
    case userType of
        "LoggedOnlineUserInfo" ->
            loggedDecoder

        "AnonymousOnlineUserInfo" ->
            anonymousDecoder

        _ ->
            D.fail <| "Unknown OnlineUserInfo type: " ++ userType


loggedDecoder : Decoder OnlineUserInfo
loggedDecoder =
    D.succeed LoggedData
        |> D.required "uuid" Uuid.decoder
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "gravatarHash" D.string
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "colorNumber" D.int
        |> D.map Logged


anonymousDecoder : Decoder OnlineUserInfo
anonymousDecoder =
    D.succeed AnonymousData
        |> D.required "avatarNumber" D.int
        |> D.required "colorNumber" D.int
        |> D.map Anonymous


getUuid : OnlineUserInfo -> Maybe Uuid
getUuid onlineUserInfo =
    case onlineUserInfo of
        Logged loggedData ->
            Just loggedData.uuid

        Anonymous _ ->
            Nothing


matchUuid : Uuid -> OnlineUserInfo -> Bool
matchUuid uuid onlineUserInfo =
    case onlineUserInfo of
        Logged loggedData ->
            loggedData.uuid == uuid

        Anonymous _ ->
            False
