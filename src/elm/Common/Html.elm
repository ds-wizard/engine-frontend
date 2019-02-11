module Common.Html exposing
    ( emptyNode
    , fa
    , linkTo
    )

import Common.Html.Attribute exposing (linkToAttributes)
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs exposing (Msg)
import Routing exposing (Route)


linkTo : Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkTo route attributes children =
    a (attributes ++ linkToAttributes route) children


emptyNode : Html msg
emptyNode =
    text ""


fa : String -> Html msg
fa icon =
    i [ class <| "fa fa-" ++ icon ] []
