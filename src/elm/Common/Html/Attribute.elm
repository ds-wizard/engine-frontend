module Common.Html.Attribute exposing
    ( detailClass
    , linkToAttributes
    , listClass
    , wideDetailClass
    )

import Common.AppState exposing (AppState)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Routes
import Routing


linkToAttributes : AppState -> Routes.Route -> List (Attribute msg)
linkToAttributes appState route =
    [ href <| Routing.toUrl appState route
    ]


detailClass : String -> Html.Attribute msg
detailClass otherClass =
    class <| "col col-detail " ++ otherClass


wideDetailClass : String -> Html.Attribute msg
wideDetailClass otherClass =
    class <| "col col-wide-detail " ++ otherClass


listClass : String -> Html.Attribute msg
listClass otherClass =
    class <| "col col-list " ++ otherClass
