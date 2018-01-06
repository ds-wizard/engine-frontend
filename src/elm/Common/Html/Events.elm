module Common.Html.Events exposing (onLinkClick)

{-| Common helpers for Html events.


# Helpers

@docs onLinkClick

-}

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode


{-| -}
onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
    onWithOptions "click" options (Decode.succeed message)
