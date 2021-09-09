module Wizard.Common.Html.Attribute exposing
    ( dataCy
    , detailClass
    , grammarlyAttributes
    , linkToAttributes
    , listClass
    , wideDetailClass
    )

import Html
import Html.Attributes exposing (attribute, class, href)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


linkToAttributes : AppState -> Routes.Route -> List (Html.Attribute msg)
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


grammarlyAttributes : List (Html.Attribute msg)
grammarlyAttributes =
    [ attribute "data-gramm" "false" ]


dataCy : String -> Html.Attribute msg
dataCy value =
    attribute "data-cy" value
