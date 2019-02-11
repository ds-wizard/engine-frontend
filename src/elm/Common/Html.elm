module Common.Html exposing
    ( detailContainerClass
    , detailContainerClassWith
    , emptyNode
    , fa
    , linkTo
    , linkToAttributes
    )

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Msgs exposing (Msg)
import Routing exposing (Route)


linkTo : Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkTo route attributes children =
    a (attributes ++ linkToAttributes route) children


linkToAttributes : Route -> List (Attribute Msg)
linkToAttributes route =
    [ href <| Routing.toUrl route
    ]


emptyNode : Html msg
emptyNode =
    text ""


detailContainerClassWith : String -> Html.Attribute msg
detailContainerClassWith otherClass =
    class <| "col-sm-12 col-md-10 col-lg-8 col-xl-6 " ++ otherClass


detailContainerClass : Html.Attribute msg
detailContainerClass =
    detailContainerClassWith ""


fa : String -> Html msg
fa icon =
    i [ class <| "fa fa-" ++ icon ] []
