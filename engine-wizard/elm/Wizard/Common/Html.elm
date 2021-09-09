module Wizard.Common.Html exposing (illustratedMessage, linkTo)

import Html exposing (Attribute, Html, a, div, img, p, text)
import Html.Attributes exposing (class, src)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (linkToAttributes)
import Wizard.Routes as Routes


linkTo : AppState -> Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo appState route attributes children =
    a (attributes ++ linkToAttributes appState route) children


illustratedMessage : String -> String -> Html msg
illustratedMessage image message =
    div [ class "illustrated-message" ]
        [ img [ src <| "/img/illustrations/undraw_" ++ image ++ ".svg" ] []
        , p [] [ text message ]
        ]
