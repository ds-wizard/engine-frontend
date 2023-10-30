module Shared.Data.BootstrapConfig.Admin exposing
    ( Admin
    , decoder
    , default
    , getClientUrl
    , isEnabled
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type Admin
    = AdminEnabled String
    | AdminDisabled


default : Admin
default =
    AdminDisabled


decoder : Decoder Admin
decoder =
    D.succeed Tuple.pair
        |> D.required "enabled" D.bool
        |> D.required "clientUrl" (D.maybe D.string)
        |> D.map
            (\( enabled, mbClientUrl ) ->
                case ( enabled, mbClientUrl ) of
                    ( True, Just clientUrl ) ->
                        AdminEnabled clientUrl

                    _ ->
                        AdminDisabled
            )


isEnabled : Admin -> Bool
isEnabled admin =
    case admin of
        AdminEnabled _ ->
            True

        _ ->
            False


getClientUrl : Admin -> Maybe String
getClientUrl admin =
    case admin of
        AdminEnabled clientUrl ->
            Just clientUrl

        _ ->
            Nothing
