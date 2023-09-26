module Wizard.Common.View.AppIcon exposing (view)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig


view : { a | logoUrl : Maybe String, primaryColor : Maybe String } -> Html msg
view app =
    div
        [ class "ItemIcon ItemIcon--App"
        ]
        [ img [ src (Maybe.withDefault LookAndFeelConfig.defaultLogoUrl app.logoUrl) ] [] ]
