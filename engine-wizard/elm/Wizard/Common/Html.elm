module Wizard.Common.Html exposing
    ( guideLink
    , illustratedMessage
    , linkTo
    , resizableTextarea
    )

import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, div, p, text, textarea)
import Html.Attributes exposing (class, href, rows, target, value)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks exposing (GuideLinks)
import Wizard.Common.Html.Attribute exposing (grammarlyAttributes, linkToAttributes, tooltipLeft)
import Wizard.Routes as Routes


linkTo : AppState -> Routes.Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo appState route attributes children =
    a (attributes ++ linkToAttributes appState route) children


guideLink : AppState -> (GuideLinks -> String) -> Html msg
guideLink appState getLink =
    a
        (href (GuideLinks.wrap (getLink appState.guideLinks))
            :: class "guide-link"
            :: target "_blank"
            :: tooltipLeft (gettext "Learn more in guide" appState.locale)
        )
        [ faSet "_global.guideLink" appState ]


illustratedMessage : Html msg -> String -> Html msg
illustratedMessage image message =
    div [ class "illustrated-message" ]
        [ image
        , p [] [ text message ]
        ]


resizableTextarea : Int -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
resizableTextarea minRows editValue attributes =
    let
        textAreaRows =
            String.split "\n" editValue
                |> List.length
                |> max minRows
    in
    textarea ([ value editValue, rows textAreaRows ] ++ grammarlyAttributes ++ attributes)
