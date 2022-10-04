module Wizard.Common.View.AppIcon exposing (view)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, classList, src)
import Maybe.Extra as Maybe


view : { a | logoUrl : Maybe String, primaryColor : Maybe String } -> Html msg
view app =
    let
        content =
            case app.logoUrl of
                Just logoUrl ->
                    [ img [ src logoUrl ] [] ]

                Nothing ->
                    []
    in
    div
        [ class "ItemIcon ItemIcon--App"
        , classList [ ( "ItemIcon--App--DefaultLogo", Maybe.isNothing app.logoUrl ) ]
        ]
        content
