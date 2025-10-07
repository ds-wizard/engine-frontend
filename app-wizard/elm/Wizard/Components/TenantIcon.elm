module Wizard.Components.TenantIcon exposing (view)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig


view : { a | logoUrl : Maybe String, primaryColor : Maybe String } -> Html msg
view tenant =
    div
        [ class "ItemIcon ItemIcon--App"
        ]
        [ img [ src (Maybe.withDefault LookAndFeelConfig.defaultLogoUrl tenant.logoUrl) ] [] ]
