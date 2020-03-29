module Wizard.Common.View.ExternalLoginButton exposing (defaultBackground, defaultColor, defaultIcon, preview, view)

import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (class, href, style)
import Shared.Html exposing (fa, faKey, faSet)
import Wizard.Common.Api.Auth as AuthApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.Partials.OpenIDServiceConfig exposing (OpenIDServiceConfig)


defaultBackground : String
defaultBackground =
    "#333"


defaultColor : String
defaultColor =
    "#fff"


defaultIcon : AppState -> String
defaultIcon appState =
    Maybe.withDefault "" <| faKey "login.externalService" appState


view : AppState -> OpenIDServiceConfig -> Html msg
view appState config =
    render
        [ href <| AuthApi.authRedirectUrl config.id appState ]
        appState
        config.name
        config.style.icon
        config.style.color
        config.style.background


preview : AppState -> String -> Maybe String -> Maybe String -> Maybe String -> Html msg
preview =
    render []


render : List (Attribute msg) -> AppState -> String -> Maybe String -> Maybe String -> Maybe String -> Html msg
render attributes appState name mbIcon mbColor mbBackground =
    let
        background =
            Maybe.withDefault defaultBackground mbBackground

        color =
            Maybe.withDefault defaultColor mbColor

        icon =
            mbIcon
                |> Maybe.map (\i -> fa i)
                |> Maybe.withDefault (faSet "login.externalService" appState)
    in
    a
        ([ class "btn btn-external-login"
         , style "color" color
         , style "background" background
         ]
            ++ attributes
        )
        [ icon, text name ]
