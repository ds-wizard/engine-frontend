module Common.Html exposing
    ( emptyNode
    , fa
    , linkTo
    )

import Common.Html.Attribute exposing (linkToAttributes)
import Html exposing (..)
import Html.Attributes exposing (class)
import Routing exposing (Route)


linkTo : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attributes children =
    a (attributes ++ linkToAttributes route) children


emptyNode : Html msg
emptyNode =
    text ""


fa : String -> Html msg
fa icon =
    i [ class <| "fa fa-" ++ icon ] []
