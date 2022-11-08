module Wizard.Common.Html.Attribute exposing
    ( dataCy
    , detailClass
    , grammarlyAttribute
    , grammarlyAttributes
    , linkToAttributes
    , listClass
    , tooltip
    , tooltipCustom
    , tooltipLeft
    , tooltipRight
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


grammarlyAttribute : Html.Attribute msg
grammarlyAttribute =
    attribute "data-gramm" "false"


dataCy : String -> Html.Attribute msg
dataCy =
    attribute "data-cy"


tooltip : String -> List (Html.Attribute msg)
tooltip =
    tooltipCustom ""


tooltipLeft : String -> List (Html.Attribute msg)
tooltipLeft =
    tooltipCustom "with-tooltip-left"


tooltipRight : String -> List (Html.Attribute msg)
tooltipRight =
    tooltipCustom "with-tooltip-right"


tooltipCustom : String -> String -> List (Html.Attribute msg)
tooltipCustom extraClass value =
    [ class "with-tooltip", class extraClass, attribute "data-tooltip" value ]
