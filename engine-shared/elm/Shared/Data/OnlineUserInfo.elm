module Shared.Data.OnlineUserInfo exposing
    ( AnonymousData
    , LoggedData
    , OnlineUserInfo(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type OnlineUserInfo
    = Logged LoggedData
    | Anonymous AnonymousData


type alias LoggedData =
    { firstName : String
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
