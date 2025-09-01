module Wizard.Utils.HtmlAttributesUtils exposing
    ( detailClass
    , linkToAttributes
    , listClass
    , settingsClass
    , wideDetailClass
    )

import Html
import Html.Attributes exposing (class, href)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


linkToAttributes : Routes.Route -> List (Html.Attribute msg)
linkToAttributes route =
    [ href <| Routing.toUrl route
    ]


detailClass : String -> Html.Attribute msg
detailClass otherClass =
    class <| "col col-detail " ++ otherClass


wideDetailClass : String -> Html.Attribute msg
wideDetailClass otherClass =
    class <| "col col-wide-detail " ++ otherClass


settingsClass : String -> Html.Attribute msg
settingsClass otherClass =
    class <| "d-flex container container-max-xxl mx-auto " ++ otherClass


listClass : String -> Html.Attribute msg
listClass otherClass =
    class <| "col col-list " ++ otherClass
