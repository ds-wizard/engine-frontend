module Common.Html exposing (..)

import Common.Html.Events exposing (onLinkClick)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Msgs exposing (Msg)
import Routing exposing (Route)


linkTo : Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkTo route attributes children =
    let
        hrefAttributes =
            [ href <| Routing.toUrl route
            , onLinkClick <| Msgs.ChangeLocation <| Routing.toUrl route
            ]
    in
    a (attributes ++ hrefAttributes) children


emptyNode : Html msg
emptyNode =
    text ""
