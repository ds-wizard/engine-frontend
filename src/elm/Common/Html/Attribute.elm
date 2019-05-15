module Common.Html.Attribute exposing (detailClass, linkToAttributes, listClass)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Routing exposing (Route)


linkToAttributes : Route -> List (Attribute msg)
linkToAttributes route =
    [ href <| Routing.toUrl route
    ]


detailClass : String -> Html.Attribute msg
detailClass otherClass =
    class <| "col col-detail " ++ otherClass


listClass : String -> Html.Attribute msg
listClass otherClass =
    class <| "col col-list " ++ otherClass
