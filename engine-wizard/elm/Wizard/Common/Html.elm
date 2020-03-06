module Wizard.Common.Html exposing (linkTo)

import Html exposing (..)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (linkToAttributes)
import Wizard.Routes as Routes


linkTo : AppState -> Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo appState route attributes children =
    a (attributes ++ linkToAttributes appState route) children
