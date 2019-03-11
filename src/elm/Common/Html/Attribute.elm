module Common.Html.Attribute exposing (detailClass, linkToAttributes)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Msgs exposing (Msg)
import Routing exposing (Route)


linkToAttributes : Route -> List (Attribute Msg)
linkToAttributes route =
    [ href <| Routing.toUrl route
    ]


detailClass : String -> Html.Attribute msg
detailClass otherClass =
    class <| "col-sm-12 col-md-10 col-lg-8 col-xl-6 " ++ otherClass
